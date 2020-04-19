;;; byte2c.el --- Emacs plus GCC  -*- lexical-binding: t -*-

;;; Copyright (C) 2018, 2020 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

(require 'byte-opt)
(require 'bytecomp)
(require 'cl-lib)

(defun byte-check--next-nonnil (array index)
  (cl-incf index)
  (while (and (< index (length array))
              (null (aref array index)))
    (cl-incf index))
  (if (>= index (length array))
      nil
    index))

(defun byte-check-lapcode (bytes lapcode arg-spec)
  (let* ((depths (make-vector (length bytes) nil))
         (ops (make-vector (length bytes) nil))
         (tags (make-vector (length bytes) nil))
         (work-list nil)
         (initial-depth (if (integerp arg-spec)
                            (+ (lsh arg-spec -8)
                               (lsh (logand arg-spec 128) -7))
                          0))
         (next-constant nil)
         (last-constant nil))
    ;; Setup.
    (while lapcode
      (let ((pc (car lapcode)))
        (cl-assert (numberp pc))
        (setf lapcode (cdr lapcode))
        (when (eq (car-safe (car lapcode)) 'TAG)
          (setf (aref tags (cadar lapcode)) pc)
          (setf lapcode (cdr lapcode)))
        (setf (aref ops pc) (car lapcode))
        (setf lapcode (cdr lapcode))))

    ;; The work list holds (PC . STACK-DEPTH) entries.
    (push (cons 0 initial-depth) work-list)
    (while work-list
      (let* ((item (pop work-list))
             (pc (car item))
             (stack-depth (cdr item))
             (op (car (aref ops pc)))
             (argument (cdr (aref ops pc))))
        ;; If the stack depth is already known at this PC, assert it,
        ;; but do no further work.
        (if (aref depths pc)
            (unless (eq (aref depths pc) stack-depth) nil
                    (message "Failed at PC=%S, %S != %S" pc
                             (aref depths pc) stack-depth))
          ;; Otherwise, we haven't visited this code yet, so store the
          ;; stack depth and compute the effect of this instruction.
          (setf (aref depths pc) stack-depth)
          (cl-incf stack-depth (byte-compile-stack-adjustment op argument))
          (setq last-constant next-constant)
          (setq next-constant nil)
          ;; Now push on the work list.  This is crazily inefficient
          ;; but who cares.
          (let* ((new-pc (and (consp argument)
                              (eq (car argument) 'TAG)
                              ;; We might see "byte-constant TAG" so
                              ;; we have to also check this.
                              (numberp (cadr argument))
                              (aref tags (cadr argument))))
                 ;; The next PC if we fall through.  Could be nil.
                 (next-pc (byte-check--next-nonnil ops pc)))
            (cond
             ((eq op 'byte-goto)
              (push (cons new-pc stack-depth) work-list))
             ((memq op '(byte-goto-if-nil byte-goto-if-not-nil))
              ;; Both branches get same depth.
              (cl-assert next-pc)
              (push (cons next-pc stack-depth) work-list)
              (push (cons new-pc stack-depth) work-list))
             ((memq op '(byte-goto-if-nil-else-pop
                         byte-goto-if-not-nil-else-pop))
              ;; Fall through sees a pop.
              (cl-assert next-pc)
              (push (cons next-pc stack-depth) work-list)
              (push (cons new-pc (1+ stack-depth)) work-list))
             ((memq op '(byte-pushcatch byte-pushconditioncase))
              (cl-assert next-pc)
              ;; On fall through there is one fewer stack element.
              (push (cons next-pc stack-depth) work-list)
              ;; Or transfer control to the exception handler.
              (push (cons new-pc (1+ stack-depth)) work-list))
             ((eq op 'byte-return)
              ;; No fall-through.
              nil)
             ;; FIXME
             ((eq op 'byte-switch)
              (cl-assert last-constant)
              (maphash (lambda (_key tag)
                         (push (cons (aref tags (cadr tag)) stack-depth)
                               work-list))
                       (car last-constant))
              (when next-pc
                (push (cons next-pc stack-depth) work-list)))
             (t
              (when (memq op '(byte-constant byte-constant2))
                (setq next-constant (car argument)))
              (when next-pc
                (push (cons next-pc stack-depth) work-list))))))))
    ;; Return something so we can pretend we know it worked.
    depths))

(defun b2c-local (depth)
  (concat "s" (int-to-string depth)))

(defmacro b2c-predicate (fn)
  `(insert "  "
           (b2c-local depth) " = " ,fn " ("
           (b2c-local depth) ") ? Qt : Qnil;\n"))

(defmacro b2c-nullary (fn)
  `(insert "  " (b2c-local (1+ depth)) " = "
           ,fn " ();\n"))

(defmacro b2c-unary (fn)
  `(insert "  " (b2c-local depth) " = "
           ,fn " (" (b2c-local depth) ");\n"))

(defmacro b2c-binary (fn)
  `(insert "  "
           (b2c-local (1- depth)) " = " ,fn " ("
           (b2c-local (1- depth)) ", "
           (b2c-local depth) ");\n"))

(defmacro b2c-push-handler (kind)
  `(insert "  {\n"
           "  struct handler *c = push_handler ("
           (b2c-local depth) ", " ,kind ");\n"
           "  if (sys_setjmp (c->jmp)) {\n"
           "    handlerlist = c->next;\n"
           "    " (b2c-local depth) " = c->val;\n"
           "    goto L" (int-to-string (caddr insn)) ";\n"
           "  }\n"
           "  }\n"))

(defun b2c-ncall (n fn depth)
  (insert "  "
          (b2c-local (- depth n -1)) " = "
          fn " (" (int-to-string n)
          ", (Lisp_Object[]) { ")
  (dotimes (i n)
    (insert (b2c-local (- depth (- n i 1))))
    (when (< i (1- n))
      (insert ", ")))
  (insert " });\n"))

(defmacro b2c-car-or-cdr (fn)
  `(insert "  if (CONSP ("
           (b2c-local depth) "))\n"
           "    " (b2c-local depth) " = " ,fn " ("
           (b2c-local depth) ");\n"
           "  else if (!NILP ("
           (b2c-local depth) "))\n"
           "    wrong_type_argument (Qlistp, "
           (b2c-local depth) ");\n"))

(defmacro b2c-arithcompare (kind)
  `(insert "  "
           (b2c-local (1- depth)) " = arithcompare ("
           (b2c-local (1- depth)) ", "
           (b2c-local depth) ", " ,kind ");\n"))

(defun b2c-constant-map (constants)
  (let ((result (make-hash-table :test 'eql)))
    (dotimes (i (length constants))
      (puthash (aref constants i) i result))
    result))

(defsubst b2c-cindex (value hash)
  (or (gethash value hash)
      ;; Super hack, byte-switch is wack.
      (and (consp value)
           (hash-table-p (car value))
           (hash-table-p (cdr value))
           (gethash (cdr value) hash))))

(defun byte2c-prologue (symbol name arglist stack-depth constants)
  (insert "#include <config.h>\n")
  (insert "#include \"lisp.h\"\n")
  (insert "#include \"buffer.h\"\n")
  (insert "#include \"syntax.h\"\n")
  (when symbol
    (insert "/* From " (symbol-name symbol) " */\n"))
  ;; Silly warning avoidance.
  (insert "Lisp_Object " name
          " (ptrdiff_t nargs, Lisp_Object *args, "
          "Lisp_Object constants);\n")
  (insert "Lisp_Object " name
          " (ptrdiff_t nargs, Lisp_Object *args, "
          "Lisp_Object constants)\n{\n")
  ;; Declarations
  (dotimes (i stack-depth)
    (insert "  Lisp_Object "
            (b2c-local (1+ i))
            ";\n"))
  (when (> (length constants) 0)
    (insert "  Lisp_Object *vectorp = &XVECTOR (constants)->contents[0];\n"))
  (insert "\n")
  ;; Argument handling.
  (let ((rest (/= (logand arglist 128) 0))
        (mandatory (logand arglist 127))
        (nonrest (lsh arglist -8)))
    (insert "  if (! (" (int-to-string mandatory) " <= nargs")
    (unless rest
      (insert " && nargs <= " (int-to-string nonrest)))
    (insert "))\n"
            "    Fsignal (Qwrong_number_of_arguments, "
            "list2 (Fcons (make_fixnum ("
            (int-to-string mandatory) "), make_fixnum ("
            (int-to-string nonrest) ")), make_fixnum (nargs)));\n")
    ;; Initialize the mandatory arguments unconditionally.
    (dotimes (i mandatory)
      (insert "  " (b2c-local (1+ i)) " = args["
              (int-to-string i) "];\n"))
    ;; Initialize optional arguments.
    (dotimes (i (- nonrest mandatory))
      (insert "  " (b2c-local (+ mandatory i 1)) " = ("
              (int-to-string (+ mandatory i)) " < nargs) ? args["
              (int-to-string (+ mandatory i)) "] : Qnil;\n"))
    (when rest
      ;; FIXME: if a &rest argument is unused by the function, it
      ;; would be nice if we could avoid this call.  (This does happen
      ;; in Emacs.)  Could we mark Flist as __attribute__((const))?
      ;; Not technically true but maybe true enough.
      ;; We could add a flag and just lazily create the list.
      (insert "  " (b2c-local (+ nonrest 1)) " = ("
              (int-to-string nonrest) " < nargs) ? Flist (nargs - "
              (int-to-string nonrest) ", args + "
              (int-to-string nonrest) ") : Qnil;\n"))))

(defun b2c-stringify (insn)
  (replace-regexp-in-string "\\*/" "*\\/" (prin1-to-string insn) nil t))

(defun byte2c (symbol name bytecode)
  (let* ((bytes (string-as-unibyte (aref bytecode 1)))
         (constants (aref bytecode 2))
         (cmap (b2c-constant-map constants))
         (lapcode (byte-decompile-bytecode bytes  constants))
         (stack-depths (byte-check-lapcode bytes lapcode (aref bytecode 0))))
    (byte2c-prologue symbol name (aref bytecode 0) (aref bytecode 3) constants)
    ;; Note that because we have stack depths already computed, we can
    ;; simply compile all the code in a linear fashion.
    (let ((last-constant nil)
          (next-constant nil))
      (while lapcode
        (let ((pc (pop lapcode))
              (insn (pop lapcode)))
          (when (eq (car insn) 'TAG)
            (insert "L" (int-to-string (cadr insn)) ":\n")
            (setq insn (pop lapcode)))
          (setq last-constant next-constant)
          (setq next-constant last-constant)
          (let ((depth (aref stack-depths pc)))
            ;; If DEPTH is nil, then we've found some dead code, which
            ;; we can just ignore.
            (if (not depth)
                (insert (format "  /* PC=%d, DEAD CODE=%s */\n"
                                pc (b2c-stringify insn)))
              (insert (format "  /* PC=%d, stack-depth=%d, insn=%s */\n"
                              pc depth (b2c-stringify insn)))
              (cl-case (car insn)
                (byte-stack-ref
                 (insert "  "
                         (b2c-local (1+ depth)) " = "
                         (b2c-local (- depth (cdr insn))) ";\n"))
                (byte-varref
                 (let ((idx (b2c-cindex (cadr insn) cmap)))
                   ;; bytecode interp inlines more here
                   (insert "  "
                           (b2c-local (1+ depth))
                           " = Fsymbol_value (vectorp["
                           (int-to-string idx)
                           "]);\n")))
                (byte-varset
                 (let ((idx (b2c-cindex (cadr insn) cmap)))
                   (insert "  set_internal (vectorp["
                           (int-to-string idx)
                           "], "
                           (b2c-local depth) ", Qnil, SET_INTERNAL_SET);\n")))
                (byte-varbind
                 (let ((idx (b2c-cindex (cadr insn) cmap)))
                   (insert "  specbind (vectorp["
                           (int-to-string idx) "], "
                           (b2c-local depth) ");\n")))
                (byte-call
                 (b2c-ncall (1+ (cdr insn)) "Ffuncall" depth))
                (byte-unbind
                 (insert "  unbind_to (SPECPDL_INDEX () - "
                         (int-to-string (cdr insn))
                         ", Qnil);\n"))
                (byte-pophandler
                 (insert "  handlerlist = handlerlist->next;\n"))
                (byte-pushcatch
                 (b2c-push-handler "CATCHER"))
                (byte-pushconditioncase
                 (b2c-push-handler "CONDITION_CASE"))

                (byte-nth
                 (b2c-binary "Fnth"))
                ;; FIXME should we peephole optimize these or are qt/qnil
                ;; constants to the compiler?
                (byte-symbolp
                 (b2c-predicate "SYMBOLP"))
                (byte-consp
                 (b2c-predicate "CONSP"))
                (byte-stringp
                 (b2c-predicate "STRINGP"))
                (byte-listp
                 (insert "  "
                         (b2c-local depth) " = (CONSP ("
                         (b2c-local depth) ") || NILP ("
                         (b2c-local depth) ")) ? Qt : Qnil;\n"))
                (byte-eq
                 (insert "  "
                         (b2c-local (1- depth)) " = EQ ("
                         (b2c-local (1- depth)) ", "
                         (b2c-local depth) ") ? Qt : Qnil;\n"))

                (byte-memq
                 (b2c-binary "Fmemq"))

                (byte-not
                 (insert "  "
                         (b2c-local depth) " = NILP ("
                         (b2c-local depth) ") ? Qt : Qnil;\n"))
                (byte-car
                 (b2c-car-or-cdr "XCAR"))
                (byte-cdr
                 (b2c-car-or-cdr "XCDR"))

                (byte-cons
                 (b2c-binary "Fcons"))

                (byte-list1
                 (b2c-unary "list1"))
                (byte-list2
                 (b2c-binary "list2"))
                (byte-list3
                 (b2c-ncall 3 "Flist" depth))
                (byte-list4
                 (b2c-ncall 4 "Flist" depth))

                (byte-length
                 (b2c-unary "Flength"))

                (byte-aref
                 (b2c-binary "Faref"))
                (byte-aset
                 (insert "  "
                         (b2c-local (- depth 2)) " = Faset ("
                         (b2c-local (- depth 2)) ", "
                         (b2c-local (- depth 1)) ", "
                         (b2c-local depth) ");\n"))

                (byte-symbol-value
                 (b2c-unary "Fsymbol_value"))
                (byte-symbol-function
                 (b2c-unary "Fsymbol_function"))

                (byte-set
                 (b2c-binary "Fset"))
                (byte-fset
                 (b2c-binary "Ffset"))
                (byte-get
                 (b2c-binary "Fget"))
                (byte-substring
                 (insert "  "
                         (b2c-local (- depth 2)) " = Fsubstring ("
                         (b2c-local (- depth 2)) ", "
                         (b2c-local (- depth 1)) ", "
                         (b2c-local depth) ");\n"))

                (byte-concat2
                 (b2c-ncall 2 "Fconcat" depth))
                (byte-concat3
                 (b2c-ncall 3 "Fconcat" depth))
                (byte-concat4
                 (b2c-ncall 4 "Fconcat" depth))

                (byte-sub1
                 (insert "  (FIXNUMP ("
                         (b2c-local depth) ") && XFIXNUM ("
                         (b2c-local depth) ") != MOST_NEGATIVE_FIXNUM) "
                         "? make_fixnum (XFIXNUM ("
                         (b2c-local depth) ") - 1) : Fsub1 ("
                         (b2c-local depth) ");\n"))
                (byte-add1
                 (insert "  (FIXNUMP ("
                         (b2c-local depth) ") && XFIXNUM ("
                         (b2c-local depth) ") != MOST_POSITIVE_FIXNUM) "
                         "? make_fixnum (XFIXNUM ("
                         (b2c-local depth) ") + 1) : Fadd1 ("
                         (b2c-local depth) ");\n"))
                (byte-eqlsign
                 ;; byte interpreter inlines here
                 (b2c-binary "Feql"))
                (byte-gtr
                 (b2c-arithcompare "ARITH_GRTR"))
                (byte-lss
                 (b2c-arithcompare "ARITH_LESS"))
                (byte-leq
                 (b2c-arithcompare "ARITH_LESS_OR_EQUAL"))
                (byte-geq
                 (b2c-arithcompare "ARITH_GRTR_OR_EQUAL"))
                (byte-diff
                 (b2c-ncall 2 "Fminus" depth))
                (byte-negate
                 (insert "  (FIXNUMP ("
                         (b2c-local depth) ") && XFIXNUM ("
                         (b2c-local depth) ") != MOST_NEGATIVE_FIXNUM) "
                         "? make_fixnum (- XFIXNUM ("
                         ;; Need a temporary
                         (b2c-local depth) ")) : Fminus (1, (Lisp_Object[]) {"
                         (b2c-local depth) "});\n"))
                (byte-plus
                 (b2c-ncall 2 "Fplus" depth))
                (byte-max
                 (b2c-ncall 2 "Fmax" depth))
                (byte-min
                 (b2c-ncall 2 "Fmin" depth))
                (byte-mult
                 (b2c-ncall 2 "Ftimes" depth))
                (byte-point
                 (insert "  "
                         (b2c-local (1+ depth)) " = make_fixed_natnum (PT);\n"))
                (byte-goto-char
                 (b2c-unary "Fgoto_char"))
                (byte-insert
                 (b2c-ncall 1 "Finsert" depth))
                (byte-point-max
                 (insert "  "
                         (b2c-local (1+ depth)) " = make_fixed_natnum (ZV);\n"))
                (byte-point-min
                 (insert "  "
                         (b2c-local (1+ depth)) " = make_fixed_natnum (BEGV);\n"))
                (byte-char-after
                 (b2c-unary "Fchar_after"))
                (byte-following-char
                 (b2c-nullary "Ffollowing_char"))
                (byte-preceding-char
                 (b2c-nullary "Fprevious_char"))
                (byte-current-column
                 (insert "  "
                         (b2c-local (1+ depth))
                         " = make_fixed_natnum (current_column ());\n"))
                (byte-indent-to
                 (insert "  "
                         (b2c-local depth) " = Findent_to ("
                         (b2c-local depth) ", Qnil);\n"))
                (byte-eolp
                 (b2c-nullary "Feolp"))
                (byte-eobp
                 (b2c-nullary "Feobp"))
                (byte-bolp
                 (b2c-nullary "Fbolp"))
                (byte-bobp
                 (b2c-nullary "Fbobp"))
                (byte-current-buffer
                 (b2c-nullary "Fcurrent_buffer"))
                (byte-set-buffer
                 (b2c-unary "Fset_buffer"))
                (byte-save-current-buffer
                 (insert "  record_unwind_current_buffer ();\n"))
                (byte-forward-char
                 (b2c-unary "Fforward_char"))
                (byte-forward-word
                 (b2c-unary "Fforward_word"))
                (byte-skip-chars-forward
                 (b2c-binary "Fskip_chars_forward"))
                (byte-skip-chars-backward
                 (b2c-binary "Fskip_chars_backward"))
                (byte-forward-line
                 (b2c-unary "Fforward_line"))
                (byte-char-syntax
                 (insert "  { CHECK_CHARACTER ("
                         (b2c-local depth) ");\n"
                         "  int c = XFIXNAT ("
                         (b2c-local depth) ");\n"
                         "  if (NILP (BVAR (current_buffer, "
                         "enable_multibyte_characters)))\n"
                         "    MAKE_CHAR_MULTIBYTE (c);\n"
                         "  " (b2c-local depth) " = "
                         "make_fixnum (syntax_code_spec[SYNTAX (c)]);\n  }\n"))
                (byte-buffer-substring
                 (b2c-binary "Fbuffer_substring"))
                (byte-delete-region
                 (b2c-binary "Fdelete_region"))
                (byte-narrow-to-region
                 (b2c-binary "Fnarrow_to_region"))
                (byte-widen
                 (b2c-nullary "Fwiden"))
                (byte-end-of-line
                 (b2c-unary "Fend_of_line"))
                ((byte-constant byte-constant2)
                 (insert "  "
                         (b2c-local (1+ depth)) " = ")
                 (setq next-constant (cadr insn))
                 ;; We emit fixnums as compile-time constants
                 ;; but nothing else.
                 (if (fixnump (cadr insn))
                     (insert "make_fixnum ("
                             (int-to-string (cadr insn))
                             ")")
                   (insert "vectorp["
                           (int-to-string (b2c-cindex (cadr insn) cmap))
                           "]"))
                 (insert ";\n"))
                (byte-goto
                 (insert "  goto L"
                         (int-to-string (caddr insn))
                         ";\n"))
                ((byte-goto-if-nil byte-goto-if-nil-else-pop)
                 (insert "  if (NILP ("
                         (b2c-local depth)
                         ")) goto L"
                         (int-to-string (caddr insn))
                         ";\n"))
                ((byte-goto-if-not-nil byte-goto-if-not-nil-else-pop)
                 (insert "  if (!NILP ("
                         (b2c-local depth)
                         ")) goto L"
                         (int-to-string (caddr insn))
                         ";\n"))
                (byte-return
                 (insert "  return "
                         (b2c-local depth)
                         ";\n"))
                (byte-discard
                 ;; Nothing
                 )
                (byte-dup
                 (insert "  "
                         (b2c-local (1+ depth)) " = "
                         (b2c-local depth) ";\n"))
                (byte-save-excursion
                 (insert "  record_unwind_protect_excursion ();\n"))
                (byte-save-restriction
                 (insert "  record_unwind_protect (save_restriction_restore,\n"
                         "       save_restriction_save ());\n"))
                (byte-catch
                 (insert "  "
                         (b2c-local (1- depth)) " = internal_catch ("
                         (b2c-local (1- depth)) ", eval_sub, "
                         (b2c-local depth) ");\n"))
                (byte-unwind-protect
                 (insert "  record_unwind_protect (FUNCTIONP ("
                         (b2c-local depth)
                         ") ? bcall0 : prog_ignore, "
                         (b2c-local depth) ");\n"))
                (byte-condition-case
                 (insert "  "
                         (b2c-local (- depth 2))
                         " = internal_lisp_condition_case ("
                         (b2c-local (- depth 2)) ", "
                         (b2c-local (- depth 1)) ", "
                         (b2c-local depth) ");\n"))
                (byte-set-marker
                 (insert "  "
                         (b2c-local (- depth 2)) " = Fset_marker ("
                         (b2c-local (- depth 2)) ", "
                         (b2c-local (- depth 1)) ", "
                         (b2c-local depth) ");\n"))
                (byte-match-beginning
                 (b2c-unary "Fmatch_beginning"))
                (byte-match-end
                 (b2c-unary "Fmatch_end"))
                (byte-upcase
                 (b2c-unary "Fupcase"))
                (byte-downcase
                 (b2c-unary "Fdowncase"))
                (byte-string=
                 (b2c-binary "Fstring_equal"))
                (byte-string<
                 (b2c-binary "Fstring_lessp"))
                (byte-equal
                 ;; Wonder if it makes sense to inline an EQ check here.
                 (b2c-binary "Fequal"))
                (byte-nthcdr
                 (b2c-binary "Fnthcdr"))
                (byte-elt
                 ;; bytecode interp inlines the consp case
                 (b2c-binary "Felt"))
                (byte-member
                 (b2c-binary "Fmemq"))
                (byte-assq
                 (b2c-binary "Fassq"))
                (byte-nreverse
                 (b2c-unary "Fnreverse"))
                (byte-setcar
                 (b2c-binary "Fsetcar"))
                (byte-setcdr
                 (b2c-binary "Fsetcdr"))
                (byte-car-safe
                 (b2c-unary "CAR_SAFE"))
                (byte-cdr-safe
                 (b2c-unary "CDR_SAFE"))
                (byte-nconc
                 (b2c-ncall 2 "Fnconc" depth))
                (byte-quo
                 (b2c-ncall 2 "Fquo" depth))
                (byte-rem
                 (b2c-binary "Frem"))
                (byte-numberp
                 (b2c-predicate "NUMBERP"))
                (byte-integerp
                 (b2c-predicate "INTEGERP"))
                (byte-listN
                 (b2c-ncall (cdr insn) "Flist" depth))
                (byte-concatN
                 (b2c-ncall (cdr insn) "Fconcat" depth))
                (byte-insertN
                 (b2c-ncall (cdr insn) "Finsert" depth))
                ((byte-stack-set byte-stack-set2 byte-discardN-preserve-tos)
                 (when (> (cdr insn) 0)
                   (insert "  "
                           (b2c-local (- depth (cdr insn)))
                           " = "
                           (b2c-local depth)
                           ";\n")))
                (byte-discardN
                 ;; Nothing
                 )
                (byte-switch
                 ;; The evilest bytecode.
                 (cl-assert last-constant)
                 (insert "  { ptrdiff_t i = hash_lookup (XHASH_TABLE ("
                         (b2c-local (1- depth)) "), "
                         (b2c-local depth) ", NULL);\n"
                         "    if (i >= 0) {\n"
                         "      Lisp_Object val = HASH_VALUE (XHASH_TABLE ("
                         (b2c-local (1- depth)) "), i);\n"
                         "      int op = XFIXNUM (val);\n"
                         "      switch (op) {\n")
                 (maphash
                  (lambda (_ignore tag)
                    (insert "      case " (int-to-string (cadr tag))
                            ": goto L"
                            (int-to-string (cadr tag))
                            ";\n"))
                  (car last-constant))
                 (insert "      }\n    }\n  }\n"))
                (t
                 (error "unrecognized byte op %S" (car insn)))))))))
    (insert "}")))

(provide 'byte2c)

;;; byte2c.el ends here
