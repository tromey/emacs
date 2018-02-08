;;; jit support

;;;###autoload
(defun jit-disassemble (func)
  (interactive "aDisassemble function: ")
  (when (symbolp func)
    (setf func (symbol-function func)))
  (let ((str (jit-disassemble-to-string func)))
    (with-current-buffer (get-buffer-create "*JIT*")
      (erase-buffer)
      (save-excursion
        (insert str))
      (pop-to-buffer (current-buffer)))))

(defvar jit--failures '())

(defun jit--try-compile-all ()
  (interactive)
  (message "================")
  (mapatoms (lambda (sym)
              (when (fboundp sym)
                (let ((func (symbol-function sym)))
                  (and
                   (byte-code-function-p func)
                   ;; FIXME - for now only lexical binding
                   (integerp (aref func 0))
                   ;; ... already loaded.
                   (not (consp (aref func 1)))
                   (progn
                     ;; (message "... %S" sym)
                     (condition-case nil
                         (jit-compile func)
                       (error
                        (push sym jit--failures)
                        (message "... %S FAIL" sym))))))))))

(defun jit--time-compile (func)
  (when (symbolp func)
    (setf func (symbol-function func))
    (let ((start (float-time)))
      (jit-compile func)
      (- (float-time) start))))

(provide 'jit-support)
