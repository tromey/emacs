#include <config.h>
#include <lisp.h>

#define exit_exception_handler() (prev_handler = *handlerlist, handlerlist = handlerlist->next)
#define pop_exception_handler() handlerlist = handlerlist->next
#define catch_value(H) ((H)->val)
#define signal_conditions(H) (XCAR ((H)->val))
#define signal_value(H) (XCDR ((H)->val))

static Lisp_Object Qplist_put;

static Lisp_Object Kwidget_put;
static Lisp_Object Kidentity;

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
  Qplist_put = intern_c_string ("plist-put");
  staticpro (&Qplist_put);

  defsubr (&Swidget_put);
  XSETSUBR (Kwidget_put, &Swidget_put);
  defsubr (&Sidentity);
  XSETSUBR (Kidentity, &Sidentity);
}
