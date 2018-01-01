;;; Some things rewritten from fns.c.

(eval-when-compile (require 'cl-macs))

(defun identity (arg)
  "Return the argument unchanged."
  arg)

(defun copy-alist (alist)
  "Return a copy of ALIST.
This is an alist which represents the same mapping from objects to objects,
but does not share the alist structure with ALIST.
The objects mapped (cars and cdrs of elements of the alist)
are shared, however.
Elements of ALIST that are not conses are also shared."
  (cl-check-type alist list)
  (cl-loop for elt in alist
	   collect (if (consp elt)
		       (cons (car elt) (cdr elt))
		     elt)))

(defun nth (n list)
  "Return the Nth element of LIST.
N counts from zero.  If LIST is not that long, nil is returned."
  (car (nthcdr n list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun featurep (feature &optional subfeature)
  "Return t if FEATURE is present in this Emacs.

Use this to conditionalize execution of lisp code based on the
presence or absence of Emacs or environment extensions.
Use `provide' to declare that a feature is available.  This function
looks at the value of the variable `features'.  The optional argument
SUBFEATURE can be used to check a specific subfeature of FEATURE."
  (cl-check-type feature symbol)
  (let ((tem (memq feature features)))
    (and tem subfeature
	 (setq tem (member subfeature (get feature 'subfeatures))))
    (if tem t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun widget-put (widget property value)
  "In WIDGET, set PROPERTY to VALUE.
The value can later be retrieved with `widget-get'."
  (setcdr widget (plist-put (cdr widget) property value)))

(defun widget-apply (widget property &rest args)
  "Apply the value of WIDGET's PROPERTY to the widget itself.
ARGS are passed as extra arguments to the function.
usage: (widget-apply WIDGET PROPERTY &rest ARGS)"
  (apply (widget-get widget property) widget args))
