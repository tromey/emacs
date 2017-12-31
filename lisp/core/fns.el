;;; Some things rewritten from fns.c.

(defun identity (arg)
  "Return the argument unchanged."
  arg)

(defun widget-put (widget property value)
  "In WIDGET, set PROPERTY to VALUE.
The value can later be retrieved with `widget-get'."
  (setcdr widget (plist-put (cdr widget) property value)))
