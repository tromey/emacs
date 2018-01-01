#include <config.h>
#include <lisp.h>

#define exit_exception_handler() (prev_handler = *handlerlist, handlerlist = handlerlist->next)
#define pop_exception_handler() handlerlist = handlerlist->next
#define catch_value(H) ((H)->val)
#define signal_conditions(H) (XCAR ((H)->val))
#define signal_value(H) (XCDR ((H)->val))

static Lisp_Object LQwidget_get;
static Lisp_Object LQplist_put;
static Lisp_Object LQnil;
static Lisp_Object LQlist;
static Lisp_Object LQalist;
static Lisp_Object LQsignal;
static Lisp_Object LQwrong_type_argument;

static Lisp_Object Kwidget_apply;
static Lisp_Object Kwidget_put;
static Lisp_Object Knth;
static Lisp_Object Kcopy_alist;
static Lisp_Object Kidentity;

DEFUN ("widget-apply", Fwidget_apply, Swidget_apply, 2, MANY,
    0,
    doc: /* Apply the value of WIDGET's PROPERTY to the widget itself.
ARGS are passed as extra arguments to the function.
usage: (widget-apply WIDGET PROPERTY &rest ARGS) */)
  (ptrdiff_t nargs90, Lisp_Object *args91)
{
  Lisp_Object widget = Qnil;
  Lisp_Object property = Qnil;
  Lisp_Object args = Qnil;
  if (nargs90 > 0)
    {
      widget = *args91++;
      --nargs90;
    }
  if (nargs90 > 0)
    {
      property = *args91++;
      --nargs90;
    }
  args = Flist (nargs90, args91);
  struct handler prev_handler;
  Lisp_Object G87_88;
  Lisp_Object G86_89;

BB_0:
  G87_88 = Ffuncall (3, ((Lisp_Object[]) { LQwidget_get, widget, property }));
  G86_89 = Fapply (3, ((Lisp_Object[]) { G87_88, widget, args }));
  return G86_89;
}

DEFUN ("widget-put", Fwidget_put, Swidget_put, 3, 3,
    0,
    doc: /* In WIDGET, set PROPERTY to VALUE.
The value can later be retrieved with `widget-get'. */)
  (Lisp_Object widget, Lisp_Object property, Lisp_Object value)
{
  struct handler prev_handler;
  Lisp_Object G82_83;
  Lisp_Object G81_84;
  Lisp_Object G80_85;

BB_0:
  G82_83 = Fcdr (widget);
  G81_84 = Ffuncall (4, ((Lisp_Object[]) { LQplist_put, G82_83, property, value }));
  G80_85 = Fsetcdr (widget, G81_84);
  return G80_85;
}

DEFUN ("nth", Fnth, Snth, 2, 2,
    0,
    doc: /* Return the Nth element of LIST.
N counts from zero.  If LIST is not that long, nil is returned. */)
  (Lisp_Object n, Lisp_Object list)
{
  struct handler prev_handler;
  Lisp_Object G77_78;
  Lisp_Object G76_79;

BB_0:
  G77_78 = Fnthcdr (n, list);
  G76_79 = Fcar (G77_78);
  return G76_79;
}

DEFUN ("copy-alist", Fcopy_alist, Scopy_alist, 1, 1,
    0,
    doc: /* Return a copy of ALIST.
This is an alist which represents the same mapping from objects to objects,
but does not share the alist structure with ALIST.
The objects mapped (cars and cdrs of elements of the alist)
are shared, however.
Elements of ALIST that are not conses are also shared. */)
  (Lisp_Object alist)
{
  struct handler prev_handler;
  Lisp_Object G3_11;
  Lisp_Object __cl_var___19;
  Lisp_Object __cl_var___22;
  Lisp_Object __cl_var___24;
  Lisp_Object G6_25;
  Lisp_Object elt_39;
  Lisp_Object G8_40;
  Lisp_Object G7_63;
  Lisp_Object G9_64;
  Lisp_Object G10_65;
  Lisp_Object G7_66;
  Lisp_Object __cl_var___69;
  Lisp_Object __cl_var___70;
  Lisp_Object G2_38;
  Lisp_Object G5_16;

BB_0:
  G3_11 = Flistp (alist);
  if (!NILP (G3_11))
    {
      goto BB_1;
    }
  else
    {
      goto BB_2;
    };
BB_1:
  __cl_var___19 = LQnil;
        __cl_var___22 = alist;
      __cl_var___24 = __cl_var___19;
  goto BB_5;
BB_5:
  G6_25 = Fconsp (__cl_var___22);
  if (!NILP (G6_25))
    {
      goto BB_7;
    }
  else
    {
      goto BB_6;
    };
BB_7:
  elt_39 = XCAR (__cl_var___22);
  G8_40 = Fconsp (elt_39);
  if (!NILP (G8_40))
    {
      goto BB_10;
    }
  else
    {
      G7_63 = elt_39;
      goto BB_9;
    };
BB_10:
  G9_64 = XCAR (elt_39);
  G10_65 = XCDR (elt_39);
  G7_66 = Fcons (G9_64, G10_65);
        G7_63 = G7_66;
  goto BB_9;
BB_9:
  __cl_var___69 = Fcons (G7_63, __cl_var___24);
  __cl_var___70 = XCDR (__cl_var___22);
        __cl_var___22 = __cl_var___70;
      __cl_var___24 = __cl_var___69;
  goto BB_5;
BB_6:
  G2_38 = Fnreverse (__cl_var___24);
  return G2_38;
BB_2:
  G5_16 = Flist (3, ((Lisp_Object[]) { LQlist, alist, LQalist }));
  Ffuncall (3, ((Lisp_Object[]) { LQsignal, LQwrong_type_argument, G5_16 }));
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
  LQwidget_get = intern_c_string ("widget-get");
  staticpro (&LQwidget_get);
  LQplist_put = intern_c_string ("plist-put");
  staticpro (&LQplist_put);
  LQnil = intern_c_string ("nil");
  staticpro (&LQnil);
  LQlist = intern_c_string ("list");
  staticpro (&LQlist);
  LQalist = intern_c_string ("alist");
  staticpro (&LQalist);
  LQsignal = intern_c_string ("signal");
  staticpro (&LQsignal);
  LQwrong_type_argument = intern_c_string ("wrong-type-argument");
  staticpro (&LQwrong_type_argument);

  defsubr (&Swidget_apply);
  XSETSUBR (Kwidget_apply, &Swidget_apply);
  defsubr (&Swidget_put);
  XSETSUBR (Kwidget_put, &Swidget_put);
  defsubr (&Snth);
  XSETSUBR (Knth, &Snth);
  defsubr (&Scopy_alist);
  XSETSUBR (Kcopy_alist, &Scopy_alist);
  defsubr (&Sidentity);
  XSETSUBR (Kidentity, &Sidentity);
}
