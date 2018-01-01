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
static Lisp_Object LQreverse;
static Lisp_Object LQcircular_list;
static Lisp_Object LQvectorp;
static Lisp_Object LQbool_vector_p;
static Lisp_Object LQarrayp;
static Lisp_Object LQlist;
static Lisp_Object LQalist;

static Lisp_Object Kwidget_apply;
static Lisp_Object Kwidget_get;
static Lisp_Object Kwidget_put;
static Lisp_Object Kfeaturep;
static Lisp_Object Knreverse;
static Lisp_Object Knth;
static Lisp_Object Kcopy_alist;
static Lisp_Object Kidentity;

DEFUN ("widget-apply", Fwidget_apply, Swidget_apply, 2, MANY,
    0,
    doc: /* Apply the value of WIDGET's PROPERTY to the widget itself.
ARGS are passed as extra arguments to the function.
usage: (widget-apply WIDGET PROPERTY &rest ARGS) */)
  (ptrdiff_t nargs450, Lisp_Object *args451)
{
  Lisp_Object widget = Qnil;
  Lisp_Object property = Qnil;
  Lisp_Object args = Qnil;
  if (nargs450 > 0)
    {
      widget = *args451++;
      --nargs450;
    }
  if (nargs450 > 0)
    {
      property = *args451++;
      --nargs450;
    }
  args = Flist (nargs450, args451);
  struct handler prev_handler;
  Lisp_Object G447_448;
  Lisp_Object G446_449;

BB_0:
  G447_448 = Ffuncall (3, ((Lisp_Object[]) { LQwidget_get, widget, property }));
  G446_449 = Fapply (3, ((Lisp_Object[]) { G447_448, widget, args }));
  return G446_449;
}

DEFUN ("widget-get", Fwidget_get, Swidget_get, 2, 2,
    0,
    doc: /* In WIDGET, get the value of PROPERTY.
The value could either be specified when the widget was created, or
later with `widget-put'. */)
  (Lisp_Object widget, Lisp_Object property)
{
  struct handler prev_handler;
  Lisp_Object G344_351;
  Lisp_Object widget_352;
  Lisp_Object property_353;
  Lisp_Object G344_354;
  struct handler *h1;
  Lisp_Object G347_384;
  Lisp_Object tmp_386;
  Lisp_Object G348_387;
  Lisp_Object tmp_440;
  Lisp_Object G349_441;
  Lisp_Object G344_370;
  Lisp_Object G344_444;
  Lisp_Object tmp_406;
  Lisp_Object widget_427;
  Lisp_Object G344_434;
  Lisp_Object G344_445;

BB_0:
  G344_351 = LQnil;
        widget_352 = widget;
      property_353 = property;
      G344_354 = G344_351;
  goto BB_4;
BB_4:
  if (!NILP (widget_352))
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
  G347_384 = Fcdr (widget_352);
  tmp_386 = Ffuncall (3, ((Lisp_Object[]) { LQplist_member, G347_384, property_353 }));
  G348_387 = Fconsp (tmp_386);
  if (!NILP (G348_387))
    {
      goto BB_14;
    }
  else
    {
      goto BB_13;
    };
BB_14:
  tmp_440 = XCDR (tmp_386);
  G349_441 = Fcar (tmp_440);
        G344_370 = G349_441;
  goto BB_2;
BB_2:
  return G344_370;
BB_1:
  G344_444 = catch_value (&prev_handler);
        G344_370 = G344_444;
  goto BB_2;
BB_13:
  tmp_406 = Fcar (widget_352);
  if (!NILP (tmp_406))
    {
      goto BB_18;
    }
  else
    {
      goto BB_19;
    };
BB_18:
  widget_427 = Fget (tmp_406, LQwidget_type);
        widget_352 = widget_427;
      property_353 = property_353;
      G344_354 = G344_354;
  goto BB_4;
BB_19:
  G344_434 = LQnil;
        G344_370 = G344_434;
  goto BB_2;
BB_9:
  G344_445 = LQnil;
        G344_370 = G344_445;
  goto BB_2;
}

DEFUN ("widget-put", Fwidget_put, Swidget_put, 3, 3,
    0,
    doc: /* In WIDGET, set PROPERTY to VALUE.
The value can later be retrieved with `widget-get'. */)
  (Lisp_Object widget, Lisp_Object property, Lisp_Object value)
{
  struct handler prev_handler;
  Lisp_Object G340_341;
  Lisp_Object G339_342;
  Lisp_Object G338_343;

BB_0:
  G340_341 = Fcdr (widget);
  G339_342 = Ffuncall (4, ((Lisp_Object[]) { LQplist_put, G340_341, property, value }));
  G338_343 = Fsetcdr (widget, G339_342);
  return G338_343;
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
  Lisp_Object G258_267;
  Lisp_Object G261_275;
  Lisp_Object tem_276;
  Lisp_Object tem_288;
  Lisp_Object G265_299;
  Lisp_Object tem_300;
  Lisp_Object G257_337;
  Lisp_Object G257_336;
  Lisp_Object G257_325;
  Lisp_Object G260_274;

BB_0:
  G258_267 = Fsymbolp (feature);
  if (!NILP (G258_267))
    {
      goto BB_1;
    }
  else
    {
      goto BB_2;
    };
BB_1:
  G261_275 = Fsymbol_value (LQfeatures);
  tem_276 = Fmemq (feature, G261_275);
  if (!NILP (tem_276))
    {
      goto BB_6;
    }
  else
    {
      tem_288 = tem_276;
      goto BB_5;
    };
BB_6:
  if (!NILP (subfeature))
    {
      goto BB_7;
    }
  else
    {
      tem_288 = tem_276;
      goto BB_5;
    };
BB_7:
  G265_299 = Fget (feature, LQsubfeatures);
  tem_300 = Fmember (subfeature, G265_299);
        tem_288 = tem_300;
  goto BB_5;
BB_5:
  if (!NILP (tem_288))
    {
      goto BB_11;
    }
  else
    {
      goto BB_9;
    };
BB_11:
  G257_337 = LQt;
        G257_336 = G257_337;
  goto BB_10;
BB_10:
  return G257_336;
BB_9:
  G257_325 = LQnil;
        G257_336 = G257_325;
  goto BB_10;
BB_2:
  G260_274 = Flist (3, ((Lisp_Object[]) { LQsymbol, feature, LQfeature }));
  Ffuncall (3, ((Lisp_Object[]) { LQsignal, LQwrong_type_argument, G260_274 }));
}

DEFUN ("nreverse", Fnreverse, Snreverse, 1, 1,
    0,
    doc: /* Reverse order of items in a list, vector or string SEQ.
If SEQ is a list, it should be nil-terminated.
This function may destructively modify SEQ to produce the value. */)
  (Lisp_Object seq)
{
  struct handler prev_handler;
  Lisp_Object G80_150;
  Lisp_Object G82_97;
  Lisp_Object G80_255;
  Lisp_Object G83_104;
  Lisp_Object prev_203;
  Lisp_Object seq_205;
  Lisp_Object prev_209;
  Lisp_Object tail_210;
  Lisp_Object G84_211;
  Lisp_Object next_230;
  Lisp_Object G85_231;
  Lisp_Object G86_113;
  Lisp_Object size_152;
  Lisp_Object __dotimes_limit___153;
  Lisp_Object i_154;
  Lisp_Object seq_155;
  Lisp_Object size_160;
  Lisp_Object __dotimes_limit___161;
  Lisp_Object i_162;
  Lisp_Object G87_163;
  Lisp_Object tem_187;
  Lisp_Object G89_190;
  Lisp_Object G88_191;
  Lisp_Object v_193;
  Lisp_Object i_194;
  Lisp_Object G86_124;
  Lisp_Object G91_151;

BB_0:
  if (!NILP (seq))
    {
      goto BB_2;
    }
  else
    {
      G80_150 = seq;
      goto BB_1;
    };
BB_2:
  G82_97 = Fstringp (seq);
  if (!NILP (G82_97))
    {
      goto BB_6;
    }
  else
    {
      goto BB_5;
    };
BB_6:
  G80_255 = Ffuncall (2, ((Lisp_Object[]) { LQreverse, seq }));
        G80_150 = G80_255;
  goto BB_1;
BB_1:
  return G80_150;
BB_5:
  G83_104 = Fconsp (seq);
  if (!NILP (G83_104))
    {
      goto BB_9;
    }
  else
    {
      goto BB_8;
    };
BB_9:
  prev_203 = LQnil;
        seq_205 = seq;
      prev_209 = prev_203;
      tail_210 = seq;
  goto BB_10;
BB_10:
  G84_211 = Fconsp (tail_210);
  if (!NILP (G84_211))
    {
      goto BB_12;
    }
  else
    {
      G80_150 = prev_209;
      goto BB_1;
    };
BB_12:
  next_230 = XCDR (tail_210);
  G85_231 = Feq (next_230, seq_205);
  if (!NILP (G85_231))
    {
      goto BB_15;
    }
  else
    {
      goto BB_14;
    };
BB_15:
  Ffuncall (3, ((Lisp_Object[]) { LQsignal, LQcircular_list, seq_205 }));
BB_14:
  Fsetcdr (tail_210, prev_209);
        seq_205 = seq_205;
      prev_209 = tail_210;
      tail_210 = next_230;
  goto BB_10;
BB_8:
  G86_113 = Ffuncall (2, ((Lisp_Object[]) { LQvectorp, seq }));
  if (!NILP (G86_113))
    {
      goto BB_24;
    }
  else
    {
      goto BB_22;
    };
BB_24:
  size_152 = Flength (seq);
  __dotimes_limit___153 = Fquo (2, ((Lisp_Object[]) { size_152, make_number (2) }));
  i_154 = make_number (0);
        seq_155 = seq;
      size_160 = size_152;
      __dotimes_limit___161 = __dotimes_limit___153;
      i_162 = i_154;
  goto BB_25;
BB_25:
  G87_163 = (XINT (i_162) < XINT (__dotimes_limit___161)) ? Qt : Qnil;
  if (!NILP (G87_163))
    {
      goto BB_27;
    }
  else
    {
      G80_150 = seq_155;
      goto BB_1;
    };
BB_27:
  tem_187 = Faref (seq_155, i_162);
  G89_190 = Fminus (3, ((Lisp_Object[]) { size_160, i_162, make_number (1) }));
  G88_191 = Faref (seq_155, G89_190);
  Faset (seq_155, i_162, G88_191);
  v_193 = Fminus (3, ((Lisp_Object[]) { size_160, i_162, make_number (1) }));
  Faset (seq_155, v_193, tem_187);
  i_194 = make_number (XINT (i_162) + 1);
        seq_155 = seq_155;
      size_160 = size_160;
      __dotimes_limit___161 = __dotimes_limit___161;
      i_162 = i_194;
  goto BB_25;
BB_22:
  G86_124 = Ffuncall (2, ((Lisp_Object[]) { LQbool_vector_p, seq }));
  if (!NILP (G86_124))
    {
      goto BB_24;
    }
  else
    {
      goto BB_31;
    };
BB_31:
  G91_151 = Flist (2, ((Lisp_Object[]) { LQarrayp, seq }));
  Ffuncall (3, ((Lisp_Object[]) { LQsignal, LQwrong_type_argument, G91_151 }));
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
  LQreverse = intern_c_string ("reverse");
  staticpro (&LQreverse);
  LQcircular_list = intern_c_string ("circular-list");
  staticpro (&LQcircular_list);
  LQvectorp = intern_c_string ("vectorp");
  staticpro (&LQvectorp);
  LQbool_vector_p = intern_c_string ("bool-vector-p");
  staticpro (&LQbool_vector_p);
  LQarrayp = intern_c_string ("arrayp");
  staticpro (&LQarrayp);
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
  defsubr (&Snreverse);
  XSETSUBR (Knreverse, &Snreverse);
  defsubr (&Snth);
  XSETSUBR (Knth, &Snth);
  defsubr (&Scopy_alist);
  XSETSUBR (Kcopy_alist, &Scopy_alist);
  defsubr (&Sidentity);
  XSETSUBR (Kidentity, &Sidentity);
}
