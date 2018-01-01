#include <config.h>
#include <lisp.h>

#define exit_exception_handler() (prev_handler = *handlerlist, handlerlist = handlerlist->next)
#define pop_exception_handler() handlerlist = handlerlist->next
#define catch_value(H) ((H)->val)
#define signal_conditions(H) (XCAR ((H)->val))
#define signal_value(H) (XCDR ((H)->val))

static Lisp_Object LQwidget_get;
static Lisp_Object LQnil;
static Lisp_Object LQdone;
static Lisp_Object LQplist_member;
static Lisp_Object LQwidget_type;
static Lisp_Object LQplist_put;
static Lisp_Object LQfeatures;
static Lisp_Object LQsubfeatures;
static Lisp_Object LQt;
static Lisp_Object LQsymbol;
static Lisp_Object LQfeature;
static Lisp_Object LQsignal;
static Lisp_Object LQwrong_type_argument;
static Lisp_Object LQlist;
static Lisp_Object LQalist;

static Lisp_Object Kwidget_apply;
static Lisp_Object Kwidget_get;
static Lisp_Object Kwidget_put;
static Lisp_Object Kfeaturep;
static Lisp_Object Knth;
static Lisp_Object Kcopy_alist;
static Lisp_Object Kidentity;

DEFUN ("widget-apply", Fwidget_apply, Swidget_apply, 2, MANY,
    0,
    doc: /* Apply the value of WIDGET's PROPERTY to the widget itself.
ARGS are passed as extra arguments to the function.
usage: (widget-apply WIDGET PROPERTY &rest ARGS) */)
  (ptrdiff_t nargs273, Lisp_Object *args274)
{
  Lisp_Object widget = Qnil;
  Lisp_Object property = Qnil;
  Lisp_Object args = Qnil;
  if (nargs273 > 0)
    {
      widget = *args274++;
      --nargs273;
    }
  if (nargs273 > 0)
    {
      property = *args274++;
      --nargs273;
    }
  args = Flist (nargs273, args274);
  struct handler prev_handler;
  Lisp_Object G270_271;
  Lisp_Object G269_272;

BB_0:
  G270_271 = Ffuncall (3, ((Lisp_Object[]) { LQwidget_get, widget, property }));
  G269_272 = Fapply (3, ((Lisp_Object[]) { G270_271, widget, args }));
  return G269_272;
}

DEFUN ("widget-get", Fwidget_get, Swidget_get, 2, 2,
    0,
    doc: /* In WIDGET, get the value of PROPERTY.
The value could either be specified when the widget was created, or
later with `widget-put'. */)
  (Lisp_Object widget, Lisp_Object property)
{
  struct handler prev_handler;
  Lisp_Object G167_174;
  Lisp_Object widget_175;
  Lisp_Object property_176;
  Lisp_Object G167_177;
  struct handler *h1;
  Lisp_Object G170_207;
  Lisp_Object tmp_209;
  Lisp_Object G171_210;
  Lisp_Object tmp_263;
  Lisp_Object G172_264;
  Lisp_Object G167_193;
  Lisp_Object G167_267;
  Lisp_Object tmp_229;
  Lisp_Object widget_250;
  Lisp_Object G167_257;
  Lisp_Object G167_268;

BB_0:
  G167_174 = LQnil;
        widget_175 = widget;
      property_176 = property;
      G167_177 = G167_174;
  goto BB_4;
BB_4:
  if (!NILP (widget_175))
    {
      goto BB_8;
    }
  else
    {
      goto BB_9;
    };
BB_8:
  h1 = push_handler (LQdone, CATCHER);
  if (sys_setjmp (h1->jmp))
    {
      eassert (handlerlist == h1);
      exit_exception_handler ();
      goto BB_1;
    }
  G170_207 = Fcdr (widget_175);
  tmp_209 = Ffuncall (3, ((Lisp_Object[]) { LQplist_member, G170_207, property_176 }));
  G171_210 = Fconsp (tmp_209);
  if (!NILP (G171_210))
    {
      goto BB_14;
    }
  else
    {
      goto BB_13;
    };
BB_14:
  tmp_263 = XCDR (tmp_209);
  G172_264 = Fcar (tmp_263);
        G167_193 = G172_264;
  goto BB_2;
BB_2:
  return G167_193;
BB_1:
  G167_267 = catch_value (&prev_handler);
        G167_193 = G167_267;
  goto BB_2;
BB_13:
  tmp_229 = Fcar (widget_175);
  if (!NILP (tmp_229))
    {
      goto BB_18;
    }
  else
    {
      goto BB_19;
    };
BB_18:
  widget_250 = Fget (tmp_229, LQwidget_type);
        widget_175 = widget_250;
      property_176 = property_176;
      G167_177 = G167_177;
  goto BB_4;
BB_19:
  G167_257 = LQnil;
        G167_193 = G167_257;
  goto BB_2;
BB_9:
  G167_268 = LQnil;
        G167_193 = G167_268;
  goto BB_2;
}

DEFUN ("widget-put", Fwidget_put, Swidget_put, 3, 3,
    0,
    doc: /* In WIDGET, set PROPERTY to VALUE.
The value can later be retrieved with `widget-get'. */)
  (Lisp_Object widget, Lisp_Object property, Lisp_Object value)
{
  struct handler prev_handler;
  Lisp_Object G163_164;
  Lisp_Object G162_165;
  Lisp_Object G161_166;

BB_0:
  G163_164 = Fcdr (widget);
  G162_165 = Ffuncall (4, ((Lisp_Object[]) { LQplist_put, G163_164, property, value }));
  G161_166 = Fsetcdr (widget, G162_165);
  return G161_166;
}

DEFUN ("featurep", Ffeaturep, Sfeaturep, 1, 2,
    0,
    doc: /* Return t if FEATURE is present in this Emacs.

Use this to conditionalize execution of lisp code based on the
presence or absence of Emacs or environment extensions.
Use `provide' to declare that a feature is available.  This function
looks at the value of the variable `features'.  The optional argument
SUBFEATURE can be used to check a specific subfeature of FEATURE. */)
  (Lisp_Object feature, Lisp_Object subfeature)
{
  struct handler prev_handler;
  Lisp_Object G81_90;
  Lisp_Object G84_98;
  Lisp_Object tem_99;
  Lisp_Object tem_111;
  Lisp_Object G88_122;
  Lisp_Object tem_123;
  Lisp_Object G80_160;
  Lisp_Object G80_159;
  Lisp_Object G80_148;
  Lisp_Object G83_97;

BB_0:
  G81_90 = Fsymbolp (feature);
  if (!NILP (G81_90))
    {
      goto BB_1;
    }
  else
    {
      goto BB_2;
    };
BB_1:
  G84_98 = Fsymbol_value (LQfeatures);
  tem_99 = Fmemq (feature, G84_98);
  if (!NILP (tem_99))
    {
      goto BB_6;
    }
  else
    {
      tem_111 = tem_99;
      goto BB_5;
    };
BB_6:
  if (!NILP (subfeature))
    {
      goto BB_7;
    }
  else
    {
      tem_111 = tem_99;
      goto BB_5;
    };
BB_7:
  G88_122 = Fget (feature, LQsubfeatures);
  tem_123 = Fmember (subfeature, G88_122);
        tem_111 = tem_123;
  goto BB_5;
BB_5:
  if (!NILP (tem_111))
    {
      goto BB_11;
    }
  else
    {
      goto BB_9;
    };
BB_11:
  G80_160 = LQt;
        G80_159 = G80_160;
  goto BB_10;
BB_10:
  return G80_159;
BB_9:
  G80_148 = LQnil;
        G80_159 = G80_148;
  goto BB_10;
BB_2:
  G83_97 = Flist (3, ((Lisp_Object[]) { LQsymbol, feature, LQfeature }));
  Ffuncall (3, ((Lisp_Object[]) { LQsignal, LQwrong_type_argument, G83_97 }));
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
  LQnil = intern_c_string ("nil");
  staticpro (&LQnil);
  LQdone = intern_c_string ("done");
  staticpro (&LQdone);
  LQplist_member = intern_c_string ("plist-member");
  staticpro (&LQplist_member);
  LQwidget_type = intern_c_string ("widget-type");
  staticpro (&LQwidget_type);
  LQplist_put = intern_c_string ("plist-put");
  staticpro (&LQplist_put);
  LQfeatures = intern_c_string ("features");
  staticpro (&LQfeatures);
  LQsubfeatures = intern_c_string ("subfeatures");
  staticpro (&LQsubfeatures);
  LQt = intern_c_string ("t");
  staticpro (&LQt);
  LQsymbol = intern_c_string ("symbol");
  staticpro (&LQsymbol);
  LQfeature = intern_c_string ("feature");
  staticpro (&LQfeature);
  LQsignal = intern_c_string ("signal");
  staticpro (&LQsignal);
  LQwrong_type_argument = intern_c_string ("wrong-type-argument");
  staticpro (&LQwrong_type_argument);
  LQlist = intern_c_string ("list");
  staticpro (&LQlist);
  LQalist = intern_c_string ("alist");
  staticpro (&LQalist);

  defsubr (&Swidget_apply);
  XSETSUBR (Kwidget_apply, &Swidget_apply);
  defsubr (&Swidget_get);
  XSETSUBR (Kwidget_get, &Swidget_get);
  defsubr (&Swidget_put);
  XSETSUBR (Kwidget_put, &Swidget_put);
  defsubr (&Sfeaturep);
  XSETSUBR (Kfeaturep, &Sfeaturep);
  defsubr (&Snth);
  XSETSUBR (Knth, &Snth);
  defsubr (&Scopy_alist);
  XSETSUBR (Kcopy_alist, &Scopy_alist);
  defsubr (&Sidentity);
  XSETSUBR (Kidentity, &Sidentity);
}
