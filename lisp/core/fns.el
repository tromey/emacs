;;; Some things rewritten from fns.c.

(defun identity (arg)
  "Return the argument unchanged."
  arg)

(defun nth (n list)
  "Return the Nth element of LIST.
N counts from zero.  If LIST is not that long, nil is returned."
  (car (nthcdr n list)))

(defun widget-put (widget property value)
  "In WIDGET, set PROPERTY to VALUE.
The value can later be retrieved with `widget-get'."
  (setcdr widget (plist-put (cdr widget) property value)))

(defun widget-apply (widget property &rest args)
  "Apply the value of WIDGET's PROPERTY to the widget itself.
ARGS are passed as extra arguments to the function.
usage: (widget-apply WIDGET PROPERTY &rest ARGS)"
  (apply (widget-get widget property) widget args))
