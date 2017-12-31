#include <config.h>
#include <lisp.h>

#define exit_exception_handler() (prev_handler = *handlerlist, handlerlist = handlerlist->next)
#define pop_exception_handler() handlerlist = handlerlist->next
#define catch_value(H) ((H)->val)
#define signal_conditions(H) (XCAR ((H)->val))
#define signal_value(H) (XCDR ((H)->val))

static Lisp_Object Qwidget_get;
static Lisp_Object Qplist_put;

static Lisp_Object Kwidget_apply;
static Lisp_Object Kwidget_put;
static Lisp_Object Kidentity;

DEFUN ("widget-apply", Fwidget_apply, Swidget_apply, 2, MANY,
    0,
    doc: /* Apply the value of WIDGET's PROPERTY to the widget itself.
ARGS are passed as extra arguments to the function.
usage: (widget-apply WIDGET PROPERTY &rest ARGS) */)
  (ptrdiff_t nargs12, Lisp_Object *args13)
{
  Lisp_Object widget = Qnil;
  Lisp_Object property = Qnil;
  Lisp_Object args = Qnil;
  if (nargs12 > 0)
    {
      widget = *args13++;
      --nargs12;
    }
  if (nargs12 > 0)
    {
      property = *args13++;
      --nargs12;
    }
  args = Flist (nargs12, args13);
  struct handler prev_handler;
  Lisp_Object G9_10;
  Lisp_Object G8_11;

BB_0:
  G9_10 = Ffuncall (3, ((Lisp_Object[]) { Qwidget_get, widget, property }));
  G8_11 = Fapply (3, ((Lisp_Object[]) { G9_10, widget, args }));
  return G8_11;
}

DEFUN ("widget-put", Fwidget_put, Swidget_put, 3, 3,
    0,
    doc: /* In WIDGET, set PROPERTY to VALUE.
The value can later be retrieved with `widget-get'. */)
  (Lisp_Object widget, Lisp_Object property, Lisp_Object value)
{
  struct handler prev_handler;
  Lisp_Object G4_5;
  Lisp_Object G3_6;
  Lisp_Object G2_7;

BB_0:
  G4_5 = Fcdr (widget);
  G3_6 = Ffuncall (4, ((Lisp_Object[]) { Qplist_put, G4_5, property, value }));
  G2_7 = Fsetcdr (widget, G3_6);
  return G2_7;
}

DEFUN ("identity", Fidentity, Sidentity, 1, 1,
    0,
    doc: /* Return the argument unchanged. */)
  (Lisp_Object arg)
{
  struct handler prev_handler;

BB_0:
  return arg;
}


void
syms_of_generated (void)
{
  Qwidget_get = intern_c_string ("widget-get");
  staticpro (&Qwidget_get);
  Qplist_put = intern_c_string ("plist-put");
  staticpro (&Qplist_put);

  defsubr (&Swidget_apply);
  XSETSUBR (Kwidget_apply, &Swidget_apply);
  defsubr (&Swidget_put);
  XSETSUBR (Kwidget_put, &Swidget_put);
  defsubr (&Sidentity);
  XSETSUBR (Kidentity, &Sidentity);
}
