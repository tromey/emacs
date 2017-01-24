;;; Core support.

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

(defun nthcdr (num list)
  "Take cdr N times on LIST, return the result."
  (cl-check-type num integer)
  (let ((i 0))
    (while (and (< i num) list)
      (setq list (cdr list))
      (setq i (1+ i)))
    list))

(defun nth (n list)
  "Return the Nth element of LIST.
N counts from zero.  If LIST is not that long, nil is returned."
  (car (nthcdr n list)))

(defun elt (sequence n)
  "Return element of SEQUENCE at index N."
  (cl-check-type n integer)
  (if (listp sequence)
      (car (nthcdr n sequence))
    ;; FIXME: CHECK_ARRAY (sequence, Qsequencep);
    (aref sequence n)))

(defun member (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.
The value is actually the tail of LIST whose car is ELT."
  (let ((tail list)
	(result nil))
    (while (and (not result) (consp tail))
      (let ((tem (car tail)))
	(when (equal elt tem)
	  (setq result elt))))
    ;; FIXME   CHECK_LIST_END (tail, list);
    result))

(defun memq (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `eq'.
The value is actually the tail of LIST whose car is ELT."
  (let ((tail list)
	(result nil))
    (while (and (not result) (consp tail))
      (let ((tem (car tail)))
	(when (eq elt tem)
	  (setq result elt))))
    ;; FIXME   CHECK_LIST_END (tail, list);
    result))

(defun memql (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `eql'.
The value is actually the tail of LIST whose car is ELT."
  (let ((tail list)
	(result nil))
    (while (and (not result) (consp tail))
      (let ((tem (car tail)))
	(when (eql elt tem)
	  (setq result elt))))
    ;; FIXME   CHECK_LIST_END (tail, list);
    result))

(defun assq (key list)
  "Return non-nil if KEY is `eq' to the car of an element of LIST.
The value is actually the first element of LIST whose car is KEY.
Elements of LIST that are not conses are ignored."
  (let ((keep-going t))
    (while (and keep-going (consp list))
      (if (and (consp (car list))
	       (eq (car (car list)) key))
	  (setq keep-going nil)
	(setq list (cdr list)))))
    ;; FIXME   CHECK_LIST_END (tail, list);
  (car list))

(defun assoc (key list)
  "Return non-nil if KEY is `equal' to the car of an element of LIST.
The value is actually the first element of LIST whose car is KEY."
  (let ((keep-going t))
    (while (and keep-going (consp list))
      (if (and (consp (car list))
	       (equal (car (car list)) key))
	  (setq keep-going nil)
	(setq list (cdr list)))))
    ;; FIXME   CHECK_LIST_END (tail, list);
  (car list))

(defun rassq (key list)
  "Return non-nil if KEY is `eq' to the cdr of an element of LIST.
The value is actually the first element of LIST whose cdr is KEY."
  (let ((keep-going t))
    (while (and keep-going (consp list))
      (if (and (consp (car list))
	       (eq (cdr (car list)) key))
	  (setq keep-going nil)
	(setq list (cdr list)))))
    ;; FIXME   CHECK_LIST_END (tail, list);
  (car list))

(defun rassoc (key list)
  "Return non-nil if KEY is `equal' to the cdr of an element of LIST.b
The value is actually the first element of LIST whose cdr is KEY."
  (let ((keep-going t))
    (while (and keep-going (consp list))
      (if (and (consp (car list))
	       (equal (cdr (car list)) key))
	  (setq keep-going nil)
	(setq list (cdr list)))))
    ;; FIXME   CHECK_LIST_END (tail, list);
  (car list))

(defun nreverse (list)
  "Reverse LIST by modifying cdr pointers.
Return the reversed list.  Expects a properly nil-terminated list."
  (let ((prev nil)
	(tail list))
    (while tail
      (let ((next (cdr tail)))
	(setcdr tail prev)
	(setq prev tail)
	(setq tail next)))
    prev))

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

(defun provide (feature subfeatures)
  "Announce that FEATURE is a feature of the current Emacs.
The optional argument SUBFEATURES should be a list of symbols listing
particular subfeatures supported in this version of FEATURE."
  (cl-check-type feature symbol)
  (cl-check-type subfeatures list)
  (when autoload-queue
    (push (cons 0 features) autoload-queue))
  (unless (memq feature features)
    (push feature features))
  (when subfeatures
    (put feature 'subfeatures subfeatures))
  ;; if (initialized) <- add back
  ;; FIXME LOADHIST_ATTACH (Fcons (Qprovide, feature));
  ;; =>
  ;; if (initialized)
  ;;   Vcurrent_load_list = Fcons (x, Vcurrent_load_list);
  (push (cons 'provide feature) current-load-list)
  (let ((tem (assq feature after-load-alist)))
    (if (consp tem)
	(mapc #'funcall (cdr tem))))
  feature)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plist-member (plist prop)
  "Return non-nil if PLIST has the property PROP.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol.
Unlike `plist-get', this allows you to distinguish between a missing
property and a property with the value nil.
The value is actually the tail of PLIST whose car is PROP."
  (while (and (consp plist)
	      (not (eq (car plist) prop)))
    (setf plist (cddr plist)))
  plist)

(defun widget-put (widget property value)
  "In WIDGET, set PROPERTY to VALUE.
The value can later be retrieved with `widget-get'."
  (setcdr widget (plist-put (cdr widget) property value)))

(defun widget-get (widget property)
  "In WIDGET, get the value of PROPERTY.
The value could either be specified when the widget was created, or
later with `widget-put'."
  (catch 'done				;FIXME - lexical catch
    (while t
      (unless widget
	(throw 'done nil))
      (let ((tmp (plist-member (cdr widget) property)))
	(when (consp tmp)
	  (setf tmp (cdr tmp))
	  (throw 'done (car tmp)))
	(setf tmp (car widget))
	(unless tmp
	  (throw 'done nil))
	(setf widget (get tmp 'widget-type))))))

(defun widget-apply (widget property &rest args)
  "Apply the value of WIDGET's PROPERTY to the widget itself.
ARGS are passed as extra arguments to the function.
usage: (widget-apply WIDGET PROPERTY &rest ARGS)"
  (apply (widget-get widget property) widget args))
