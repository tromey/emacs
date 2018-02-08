;;; byte-check.el --- checking of Lisp byte code -*- lexical-binding: t -*-

;;; Check byte compiled code stack depths.

;; Test to see if Emacs byte code uses the stack consistently.  If
;; stack depths are always consistent at each opcode, we can do a
;; better sort of JIT compilation.

(require 'byte-opt)
(require 'bytecomp)

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
                          0)))
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
             (op-value (symbol-value op))
             (argument (cdr (aref ops pc)))
             (stack-effect (aref byte-stack+-info op-value)))
        ;; If the stack depth is already known at this PC, assert it,
        ;; but do no further work.
        (if (aref depths pc)
            (unless (eq (aref depths pc) stack-depth) nil
                    (message "Failed at %S PC=%S, %S != %S" sym pc
                             (aref depths pc) stack-depth))
          ;; Otherwise, we haven't visited this code yet, so store the
          ;; stack depth and compute the effect of this instruction.
          (setf (aref depths pc) stack-depth)
          (cond
           ;; FIXME byte-call has a non-nil stack effect in the
           ;; byte-stack+-info vector - why?
           ((eq op 'byte-call)
            (cl-decf stack-depth argument))
           ((numberp stack-effect)
            (cl-incf stack-depth stack-effect))
           ((memq op '(byte-listN byte-concatN byte-insertN))
            (cl-decf stack-depth (1- argument)))
           ((memq op '(byte-discardN byte-discardN-preserve-tos))
            (cl-assert (<= argument #x80))
            (cl-decf stack-depth argument))
           (t
            (error "unhandled case %S" op)))
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
              (error "byte-switch not yet handled"))
             (t
              (when next-pc
                (push (cons next-pc stack-depth) work-list))))))))
    ;; Return something so we can pretend we know it worked.
    depths))

(defun byte-check-symbol (sym)
  ;; (message "Checking %S" sym)
  (let* ((code (symbol-function sym))
         (bytes (string-as-unibyte (aref code 1)))
         (lapcode (byte-decompile-bytecode bytes (aref code 2))))
    (byte-check-lapcode bytes lapcode)))

(defun byte-check-symbols ()
  "Check all loaded bytecode for stack depth consistency."
  (interactive)
  (message "================")
  (mapatoms (lambda (sym)
              (and (fboundp sym)
                   (byte-code-function-p (symbol-function sym))
                   (not (consp (aref (symbol-function sym) 1)))
                   (byte-check-symbol sym)))))

(provide 'byte-check)

;;; byte-check.el ends here
