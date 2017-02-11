;;; mhtml-mode.el --- HTML editing mode that handles CSS and JS -*- lexical-binding:t -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Keywords: wp, hypermedia, comm, languages

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(eval-and-compile
  (require 'sgml-mode))
(require 'js)
(require 'css-mode)
(require 'prog-mode)
(require 'font-lock)

(cl-defstruct mhtml--submode
  ;; Name of this submode.
  name
  ;; HTML end tag.
  end-tag
  ;; Syntax table.
  syntax-table
  ;; Propertize function.
  propertize
  ;; Keymap.
  keymap
  ;; Captured local variables.
  captured-locals)

(defconst mhtml--variable-prefix
  (regexp-opt '("comment-" "uncomment-" "font-lock-" "electric-indent-"
                "smie-" "forward-sexp-function" "indent-line-function"
                "major-mode"))
  "Regexp matching the prefix of buffer-locals we want to capture.")

(defun mhtml--construct-submode (mode &rest args)
  "A wrapper for make-mhtml--submode that computes the buffer-local variables."
  (let ((captured-locals nil)
        (submode (apply #'make-mhtml--submode args)))
    (with-temp-buffer
      (funcall mode)
      ;; Make sure font lock is all set up.
      (font-lock-set-defaults)
      (dolist (iter (buffer-local-variables))
        (when (string-match mhtml--variable-prefix (symbol-name (car iter)))
          (push iter captured-locals)))
      (setf (mhtml--submode-captured-locals submode) captured-locals))
    submode))

(defun mhtml--mark-buffer-locals (submode)
  (dolist (iter (mhtml--submode-captured-locals submode))
    (make-local-variable (car iter))))

(defconst mhtml--css-submode
  (mhtml--construct-submode 'css-mode
                            :name "CSS"
                            :end-tag "</style>"
                            :syntax-table css-mode-syntax-table
                            :propertize css-syntax-propertize-function
                            :keymap css-mode-map))

(defconst mhtml--js-submode
  (mhtml--construct-submode 'js-mode
                            :name "JS"
                            :end-tag "</script>"
                            :syntax-table js-mode-syntax-table
                            :propertize #'js-syntax-propertize
                            :keymap js-mode-map))

(defmacro mhtml--with-locals (submode &rest body)
  (declare (indent 1))
  `(cl-progv
       (when submode (mapcar #'car (mhtml--submode-captured-locals submode)))
       (when submode (mapcar #'cdr (mhtml--submode-captured-locals submode)))
     ,@body))

(defun mhtml--submode-lighter ()
  "Mode-line lighter indicating the current submode."
  (let ((submode (get-text-property (point) 'mhtml-submode)))
    (if submode
        (mhtml--submode-name submode)
      "")))

(defun mhtml--submode-fontify-one-region (submode beg end &optional loudly)
  (if submode
      (mhtml--with-locals submode
        (save-restriction
          (narrow-to-region beg end)
          (font-lock-set-defaults)
          (font-lock-default-fontify-region (point-min) (point-max) loudly)))
    (font-lock-set-defaults)
    (font-lock-default-fontify-region beg end loudly)))

(defun mhtml--submode-fontify-region (beg end loudly)
  (while (< beg end)
    (let ((submode (get-text-property beg 'mhtml-submode))
          (this-end (next-single-property-change beg 'mhtml-submode
                                                 nil end)))
      (mhtml--submode-fontify-one-region submode beg this-end loudly)
      (setq beg this-end))))

(defun mhtml--syntax-propertize-submode (submode end)
  (save-excursion
    (when (search-forward (mhtml--submode-end-tag submode) end t)
      (setq end (match-beginning 0))))
  (set-text-properties (point) end
                       (list 'mhtml-submode submode
                             'syntax-table (mhtml--submode-syntax-table submode)
                             ;; We want local-map here so that we act
                             ;; more like the sub-mode and don't
                             ;; override minor mode maps.
                             'local-map (mhtml--submode-keymap submode)
                             'cursor-sensor-functions
                             (list (lambda (_window _old-point _action)
                                     (force-mode-line-update)))))
  (funcall (mhtml--submode-propertize submode) (point) end)
  (goto-char end))

(defun mhtml-syntax-propertize (start end)
  (goto-char start)
  (when (get-text-property (point) 'mhtml-submode)
    (mhtml--syntax-propertize-submode (get-text-property (point) 'mhtml-submode)
                                      end))
  (funcall
   (syntax-propertize-rules
    ("<style.*?>"
     (0 (ignore
         (goto-char (match-end 0))
         (mhtml--syntax-propertize-submode mhtml--css-submode end))))
    ("<script.*?>"
     (0 (ignore
         (goto-char (match-end 0))
         (mhtml--syntax-propertize-submode mhtml--js-submode end))))
    sgml-syntax-propertize-rules)
   ;; Make sure to handle the situation where
   ;; mhtml--syntax-propertize-submode moved point.
   (point) end))

(defun mhtml-indent-line ()
  "Indent the current line as HTML, JS, or CSS, according to its context."
  (interactive)
  (let ((submode (save-excursion
                   (back-to-indentation)
                   (get-text-property (point) 'mhtml-submode))))
    (if submode
        (save-restriction
          (let* ((region-start (previous-single-property-change (point)
                                                                'mhtml-submode))
                 (base-indent (save-excursion
                                (goto-char region-start)
                                (sgml-calculate-indent))))
            (narrow-to-region region-start (point-max))
            (let ((prog-indentation-context (list base-indent
                                                  (cons (point-min) nil)
                                                  nil)))
              (mhtml--with-locals submode
                ;; indent-line-function was rebound by
                ;; mhtml--with-locals.
                (funcall indent-line-function)))))
      ;; HTML.
      (sgml-indent-line))))

(defun mhtml--set-comment-function (sym)
  (let ((captured-value (symbol-value sym)))
    (set (make-local-variable sym)
         `(lambda (&rest args)
            (let ((submode (get-text-property (point) 'mhtml-submode))
                  (,sym ,captured-value))
              (mhtml--with-locals submode
                (apply ,sym args)))))))

;;;###autoload
(define-derived-mode mhtml-mode html-mode
  '((sgml-xml-mode "XHTML+" "HTML+") (:eval (mhtml--submode-lighter)))
  "Major mode based on `html-mode', but works with embedded JS and CSS.

Code inside a <script> element is indented using the rules from
`js-mode'; and code inside a <style> element is indented using
the rules from `css-mode'."
  (cursor-sensor-mode)
  (setq-local indent-line-function #'mhtml-indent-line)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local syntax-propertize-function #'mhtml-syntax-propertize)
  (setq-local font-lock-fontify-region-function
              #'mhtml--submode-fontify-region)

  ;; Make any captured variables buffer-local.
  (mhtml--mark-buffer-locals mhtml--css-submode)
  (mhtml--mark-buffer-locals mhtml--js-submode)

  (mhtml--set-comment-function 'comment-indent-function)
  ;; (mhtml--set-comment-function 'comment-insert-comment-function)
  (mhtml--set-comment-function 'comment-region-function)
  (mhtml--set-comment-function 'uncomment-region-function)
  (mhtml--set-comment-function 'comment-quote-nested-function)

  ;: Hack
  (js--update-quick-match-re)

  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-multiline 'append 'local))

(provide 'mhtml-mode)

;;; mhtml-mode.el ends here
