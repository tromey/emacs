;;; bytecode-driver.el --- compiler driver  -*- lexical-binding: t -*-

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

(require 'byte2c)
(require 'cl-lib)

(defconst bytecode-driver-object-hash-dir "src/elo/"
  "Where the files live, relative to the build root.")

(defconst bytecode-driver-object-hash-file
  (expand-file-name "hash.el" bytecode-driver-object-hash-dir)
  "Where the object hash file lives, relative to the build root.")

(defvar bytecode-driver-object-hash nil
  "The object hash table.
A key in this table is a bytecode object.
A value is the serial number of the bytecode object,
and NAME is the object file name, relative to
 `bytecode-driver-object-hash-file'.")

(defvar bytecode-driver-object-counter 0
  "The next serial number to be handed out.")

(defvar bytecode-driver-work-list nil
  "A list of functions to compile.
The list will be terminated with the keyword `:done'.")

(defun bytecode-driver-hash-test (a b)
  "Equality test for the hash table.
Compares two bytecode objects, excluding their doc string values."
  (and (equal (aref a 0) (aref b 0))
       (equal (aref a 1) (aref b 1))
       (equal (aref a 2) (aref b 2))
       (equal (aref a 3) (aref b 3))
       ;; FIXME COMPILED_INTERACTIVE ?  Maybe not all bytecode vectors
       ;; have this slot
       ))

(defun bytecode-driver-hash-hash (code)
  "Hash function for the hash table.
Computes the hash code of a bytecode object, excluding its
doc string value."
  (funcall #'+
         (sxhash (aref code 0))
         (sxhash (aref code 1))
         (sxhash (aref code 2))
         (sxhash (aref code 3))
         ;; fixme COMPILED_INTERACTIVE ?
         ))

(define-hash-table-test 'bytecode-driver-hash
  #'bytecode-driver-hash-test #'bytecode-driver-hash-hash)

(defun bytecode-driver-load-object-hash ()
  "Read the object hash table."
  (when (file-exists-p bytecode-driver-object-hash-file)
    (with-temp-buffer
      (insert-file-contents bytecode-driver-object-hash-file)
      (goto-char (point-min))
      (let ((desc (read (current-buffer))))
        (setq bytecode-driver-object-counter (car desc))
        (setq bytecode-driver-object-hash (cdr desc)))))
  (unless (hash-table-p bytecode-driver-object-hash)
    (setq bytecode-driver-object-hash (make-hash-table
                                       :test 'bytecode-driver-hash))))

(defun bytecode-driver-write-object-hash ()
  "Write the object hash table."
  (let ((print-level nil)
        (print-quoted t)
        (print-circle t)
        (print-length nil))
    (write-region
     (concat ";;; Generated by bytecode-driver.el\n"
             (prin1-to-string (cons bytecode-driver-object-counter
                                    bytecode-driver-object-hash)))
     nil
     bytecode-driver-object-hash-file)))

(defun bytecode-driver-object-name (serial)
  (format "%d.c" serial))

(defun bytecode-driver-function-name (serial)
  (format "J%d" serial))

(defun bytecode-driver-one-function (symbol bytecode)
  "Compile a single function.
Writes the object code to the appropriate location.
Updates `bytecode-driver-object-hash'."
  (let* ((count bytecode-driver-object-counter)
         (function-name (bytecode-driver-function-name count))
         (object-name (bytecode-driver-object-name count))
         (full-object-name (expand-file-name object-name
                                             bytecode-driver-object-hash-dir)))
    (cl-incf bytecode-driver-object-counter)
    (with-temp-buffer
      (byte2c symbol function-name bytecode)
      (write-region nil nil full-object-name))
    (puthash bytecode count bytecode-driver-object-hash)))

(defun bytecode-driver-write-link-file (bytecodes)
  "Write the .c file for the runtime linker."
  (with-temp-buffer
    ;; More than one bytecode object might correspond to a single
    ;; compiled function, because they might differ only in their
    ;; docstring index, which is ignored.  So here we make a list that
    ;; maps from doc string index to function name, then sort it in
    ;; ascending order, and then write some C code to make a table
    ;; that can be used at runtime.
    (insert "#include <config.h>\n")
    (insert "#include \"lisp.h\"\n")
    (let ((items
           (sort (delq nil
                       (mapcar
                        (lambda (bytecode)
                          (let* ((serial
                                  (gethash bytecode
                                           bytecode-driver-object-hash)))
                            (when (and serial
                                       (stringp (aref bytecode 1))
                                       (>= (length bytecode) 5)
                                       (integerp (aref bytecode 4)))
                              ;; Map from the function's doc string
                              ;; index to the name of the function.
                              (cons (aref bytecode 4)
                                    (bytecode-driver-function-name serial)))))
                        bytecodes))
                 (lambda (a b)
                   (< (car a) (car b))))))
      ;; First declare them.
      (maphash
       (lambda (_ignore item)
         (insert "extern Lisp_Object " (cdr item)
                 " (ptrdiff_t, Lisp_Object *, Lisp_Object);\n"))
       bytecode-driver-object-hash)
      (insert "\n")
      ;; Now the table.
      (insert "struct function_map compiled_elisp_functions[] = {\n")
      (dolist (item items)
        (insert "  { " (number-to-string (car item)) ", " (cdr item) "},\n"))
      (insert "};\n"))
    (write-region nil nil
                  (expand-file-name "link.c"
                                    bytecode-driver-object-hash-dir))))

(defun bytecode-driver-all ()
  "Compile all loaded bytecode functions to object files."
  (unless (file-exists-p bytecode-driver-object-hash-dir)
    (make-directory bytecode-driver-object-hash-dir))
  (bytecode-driver-load-object-hash)
  ;; We are going to populate a new hash table with the results, but
  ;; we want the old one around for reference and cleanup.
  (let ((old-hash bytecode-driver-object-hash)
        ;; Because multiple bytecode objects might share a compiled
        ;; function, we have to keep a list of the actual bytecode
        ;; objects we have compiled.
        (all-bytecodes nil))
    (setq bytecode-driver-object-hash (make-hash-table
                                       :test 'bytecode-driver-hash))
    ;; See what functions must still be compiled.
    (mapatoms
     (lambda (sym)
       (let ((code (symbol-function sym)))
         (when (and (byte-code-function-p code)
                    (stringp (aref code 1))
                    ;; Only lexical-binding functions.
                    (integerp (aref code 0)))
           (push code all-bytecodes)
           (let ((hval (gethash code old-hash)))
             (if hval
                 (progn
                   ;; Move the entry from the old hash to the new.
                   (remhash code old-hash)
                   (puthash code hval bytecode-driver-object-hash))
               ;; The function hasn't been compiled yet, so do it now.
               (message "Compiling %S" sym)
               (bytecode-driver-one-function sym code)))))))
    ;; (bytecode-driver-write-object-hash)
    ;; Now the old hash table only contains entries that are no longer
    ;; needed.  So, delete those object files.
    (maphash
     (lambda (_code value)
       (delete-file (expand-file-name (bytecode-driver-object-name value)
                                      bytecode-driver-object-hash-dir)))
     old-hash)
    ;; Finally, write out the C file that lets us associate bytecode
    ;; with compiled code.
    (bytecode-driver-write-link-file all-bytecodes)))

(provide 'bytecode-driver)

;;; bytecode-driver.el ends here
