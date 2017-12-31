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
static Lisp_Object Knth;
static Lisp_Object Kidentity;

DEFUN ("widget-apply", Fwidget_apply, Swidget_apply, 2, MANY,
    0,
    doc: /* Apply the value of WIDGET's PROPERTY to the widget itself.
ARGS are passed as extra arguments to the function.
usage: (widget-apply WIDGET PROPERTY &rest ARGS) */)
  (ptrdiff_t nargs16, Lisp_Object *args17)
{
  Lisp_Object widget = Qnil;
  Lisp_Object property = Qnil;
  Lisp_Object args = Qnil;
  if (nargs16 > 0)
    {
      widget = *args17++;
      --nargs16;
    }
  if (nargs16 > 0)
    {
      property = *args17++;
      --nargs16;
    }
  args = Flist (nargs16, args17);
  struct handler prev_handler;
  Lisp_Object G13_14;
  Lisp_Object G12_15;

BB_0:
  G13_14 = Ffuncall (3, ((Lisp_Object[]) { Qwidget_get, widget, property }));
  G12_15 = Fapply (3, ((Lisp_Object[]) { G13_14, widget, args }));
  return G12_15;
}

DEFUN ("widget-put", Fwidget_put, Swidget_put, 3, 3,
    0,
    doc: /* In WIDGET, set PROPERTY to VALUE.
The value can later be retrieved with `widget-get'. */)
  (Lisp_Object widget, Lisp_Object property, Lisp_Object value)
{
  struct handler prev_handler;
  Lisp_Object G8_9;
  Lisp_Object G7_10;
  Lisp_Object G6_11;

BB_0:
  G8_9 = Fcdr (widget);
  G7_10 = Ffuncall (4, ((Lisp_Object[]) { Qplist_put, G8_9, property, value }));
  G6_11 = Fsetcdr (widget, G7_10);
  return G6_11;
}

DEFUN ("nth", Fnth, Snth, 2, 2,
    0,
    doc: /* Return the Nth element of LIST.
N counts from zero.  If LIST is not that long, nil is returned. */)
  (Lisp_Object n, Lisp_Object list)
{
  struct handler prev_handler;
  Lisp_Object G3_4;
  Lisp_Object G2_5;

BB_0:
  G3_4 = Fnthcdr (n, list);
  G2_5 = Fcar (G3_4);
  return G2_5;
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
  defsubr (&Snth);
  XSETSUBR (Knth, &Snth);
  defsubr (&Sidentity);
  XSETSUBR (Kidentity, &Sidentity);
}
