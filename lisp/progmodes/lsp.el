;;; lsp.el --- Language Server Protocol -*- lexical-binding: t -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

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

;; NOTES
;; why is VersionedTextDocumentIdentifier separate
;;    what about the one in TextDocumentIItem
;; why does TextDocumentPositionParams exist
;; TextDocumentPositionParams
;; What glob patterns are supported in DocumentFilter

(require 'json)

(cl-defstruct lsp-json--connection
  process
  (next-id 0)
  pending-responses)

(defun lsp-json-send (connection method &rest args)
  (let* ((id (cl-incf (lsp-json--connection-next-id connection)))
         (obj (list :lsp-json "2.0"
                    :method method
                    :params args
                    :id id))
         (payload (json-encode obj)))
    (blah payload)
    id))

(defun lsp-json-notify (connection method &rest args)
  (let* ((id (cl-incf (lsp-json--connection-next-id connection)))
         (obj (list :lsp-json "2.0"
                    :method method
                    :params args))
         (payload (json-encode obj)))
    (blah payload)
    nil))

(defun lsp-json-cancel (connection id)
  fixme)

(defun lsp-make-position (where)
  ;; save-restriction and widen?
  (save-excursion
    (goto-char where)
    (list :line (1- (line-number-at-pos)) :column (current-column))))

(defun lsp-make-range (from to)
  (list :start (lsp-make-position from)
        :end (lsp-make-position to)))

(defun lsp-document-uri (buffer)
  fixme)

(defun lsp-make-position (where from to)
  (list :uri (lsp-document-uri (current-buffer))
        :range (lsp-make-range from to)))

(defun lsp-make-text-document (buffer)
  ;; languageId?
  (with-current-buffer buffer
    (list :uri (lsp-document-uri (current-buffer))
          :languageId FIXME
          :version FIXME
          ;; FIXME maybe we should make the low layers handle this
          ;; more efficiently
          :text (buffer-substring-no-properties (point-min) (point-max)))))



;;; lsp.el ends here
