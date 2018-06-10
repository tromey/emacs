/* Execution of byte code produced by bytecomp.el.
   Copyright (C) 2018 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>

#ifdef HAVE_LIBJIT

#include "lisp.h"
#include "buffer.h"
#include "bytecode.h"
#include "window.h"

#include <stdio.h>
#include <jit/jit.h>
#include <jit/jit-dump.h>

static bool emacs_jit_compile_nolock (Lisp_Object func);


static bool emacs_jit_initialized;

jit_context_t emacs_jit_context;

static jit_type_t nullary_signature;
static jit_type_t unary_signature;
static jit_type_t binary_signature;
static jit_type_t ternary_signature;
static jit_type_t unbind_n_signature;
static jit_type_t temp_output_buffer_show_signature;
static jit_type_t arithcompare_signature;
static jit_type_t callN_signature;
static jit_type_t compiled_signature;
static jit_type_t wrong_number_of_arguments_signature;
static jit_type_t set_internal_signature;
static jit_type_t specbind_signature;
static jit_type_t save_excursion_save_signature;
static jit_type_t record_unwind_protect_signature;
static jit_type_t void_void_signature;
static jit_type_t push_handler_signature;
static jit_type_t setjmp_signature;

struct entry_point
{
  /* The function object.  */
  jit_function_t function;
  /* The entry point for this variant.  If NULL, no entry point has
     been defined.  */
  void *entry;
};

/* One for each number of arguments, plus MANY.  */
#define N_FUNCTION_SLOTS (SUBR_MAX_ARGS + 1)

struct emacs_jit_functions
{
  struct entry_point entries[N_FUNCTION_SLOTS];
};

static jit_type_t subr_signature[N_FUNCTION_SLOTS];

static jit_type_t ptrdiff_t_type;

struct op_state
{
  bool is_branch_target;
  bool is_fallthrough_target;
  bool seen;
  bool stack_initialized;
  int stack_depth;
  /* A stack is only allocated if STACK_DEPTH is nonzero.  Each
     element is the index into the constant pool of the value, or -1
     if the value is not a constant.  */
  int *stack;

  /* Used in pass 2.  */
  jit_label_t label;
};


/* Make a pointer constant.  */
#define CONSTANT(FUNC, VAL) \
  jit_value_create_long_constant (FUNC, jit_type_void_ptr, (jit_long) (VAL))

/* Fetch the next byte from the bytecode stream.  */

#define FETCH (bytestr_data[pc++])

/* Fetch two bytes from the bytecode stream and make a 16-bit number
   out of them.  */

#define FETCH2 (op = FETCH, op + (FETCH << 8))

#define PUSH(VALUE) \
  jit_insn_store (func, stack[++stack_pointer], VALUE)

#define POP stack[stack_pointer--]

/* Discard n values from the execution stack.  */

#define DISCARD(n) (stack_pointer -= (n))

/* Get the value which is at the top of the execution stack, but don't
   pop it.  */

#define TOP stack[stack_pointer]

/* Compile code that extracts the type from VAL.  */

static jit_value_t
get_type (jit_function_t func, jit_value_t val)
{
#if USE_LSB_TAG
  jit_value_t mask = jit_value_create_nint_constant (func, jit_type_void_ptr,
						     ~VALMASK);
  return jit_insn_and (func, val, mask);
#else /* USE_LSB_TAG */
  jit_value_t shift = jit_value_create_nint_constant (func, jit_type_uint,
						      VALBITS);
  return jit_insn_ushr (func, val, shift);
#endif /* not USE_LSB_TAG */
}

static jit_value_t
untag (jit_function_t func, jit_value_t val, EMACS_UINT utype)
{
  jit_value_t tem;

  utype = utype << (USE_LSB_TAG ? 0 : VALBITS);

  tem = jit_value_create_nint_constant (func, jit_type_void_ptr, utype);
  return jit_insn_sub (func, val, tem);
}

static jit_value_t
to_int (jit_function_t func, jit_value_t val)
{
  jit_value_t shift = jit_value_create_nint_constant (func, jit_type_uint,
						      INTTYPEBITS);
#if !USE_LSB_TAG
  val = jit_insn_shl (func, val, shift);
#endif
  return jit_insn_sshr (func, val, shift);
}

static jit_value_t
eq_nil (jit_function_t func, jit_value_t val)
{
  jit_value_t nilval = CONSTANT (func, Qnil);
  return jit_insn_eq (func, val, nilval);
}

static jit_value_t
compare_type (jit_function_t func, jit_value_t val, int to_be_checked)
{
  jit_value_t real_type = get_type (func, val);
  jit_value_t type_val
    = jit_value_create_nint_constant (func, jit_type_void_ptr, to_be_checked);
  return jit_insn_eq (func, real_type, type_val);
}

static void
emit_qnil_or_qt (jit_function_t func, jit_value_t compare,
		 jit_value_t dest, jit_label_t *next_insn)
{
  jit_value_t tem;

  jit_label_t nil_label = jit_label_undefined;
  jit_insn_branch_if_not (func, compare, &nil_label);
  tem = CONSTANT (func, Qt);
  jit_insn_store (func, dest, tem);
  jit_insn_branch (func, next_insn);
  jit_insn_label (func, &nil_label);
  tem = CONSTANT (func, Qnil);
  jit_insn_store (func, dest, tem);
}

static jit_value_t
compile_nullary (jit_function_t func, const char *name,
		 Lisp_Object (*callee) (void))
{
  return jit_insn_call_native (func, name, (void *) callee,
			       nullary_signature, NULL, 0,
			       JIT_CALL_NOTHROW);
}

static void

compile_unary (jit_function_t func, const char *name,
	       Lisp_Object (*callee) (Lisp_Object),
	       jit_value_t arg_and_dest)
{
  jit_value_t result = jit_insn_call_native (func, name, (void *) callee,
					     unary_signature, &arg_and_dest, 1,
					     JIT_CALL_NOTHROW);
  jit_insn_store (func, arg_and_dest, result);
}

static void
compile_binary (jit_function_t func, const char *name,
		Lisp_Object (*callee) (Lisp_Object, Lisp_Object),
		jit_value_t arg_and_dest, jit_value_t arg2)
{
  jit_value_t args[2] = { arg_and_dest, arg2 };

  jit_value_t result = jit_insn_call_native (func, name, (void *) callee,
					     binary_signature, args, 2,
					     JIT_CALL_NOTHROW);
  jit_insn_store (func, arg_and_dest, result);
}

static void
compile_ternary (jit_function_t func, const char *name,
		 Lisp_Object (*callee) (Lisp_Object, Lisp_Object, Lisp_Object),
		 jit_value_t arg_and_dest, jit_value_t arg2, jit_value_t arg3)
{
  jit_value_t args[3] = { arg_and_dest, arg2, arg3 };

  jit_value_t result = jit_insn_call_native (func, name, (void *) callee,
					     ternary_signature, args, 3,
					     JIT_CALL_NOTHROW);
  jit_insn_store (func, arg_and_dest, result);
}

static void
compile_arithcompare (jit_function_t func, const char *name,
		      jit_value_t arg_and_dest, jit_value_t arg2, int arg3)
{
  jit_value_t tem
    = jit_value_create_nint_constant (func, jit_type_sys_int, arg3);
  jit_value_t args[3] = { arg_and_dest, arg2, tem };

  jit_value_t result = jit_insn_call_native (func, name, (void *) arithcompare,
					     arithcompare_signature, args, 3,
					     JIT_CALL_NOTHROW);
  jit_insn_store (func, arg_and_dest, result);
}

static jit_value_t
compile_make_natnum (jit_function_t func, jit_value_t untagged_int)
{
#if USE_LSB_TAG
  jit_value_t nbits
    = jit_value_create_nint_constant (func, jit_type_int, INTTYPEBITS);
  jit_value_t val = jit_insn_shl (func, untagged_int, nbits);
  jit_value_t tag
    = jit_value_create_nint_constant (func, jit_type_void_ptr, Lisp_Int0);
  return jit_insn_add (func, val, tag);
#else /* USE_LSB_TAG */
  jit_value_t tag
    = jit_value_create_nint_constant (func, jit_type_void_ptr,
				      ((EMACS_INT) Lisp_Int0) << VALBITS);
  return jit_insn_add (func, untagged_int, tag);
#endif /* not USE_LSB_TAG */
}

static jit_value_t
compile_make_number (jit_function_t func, jit_value_t untagged_int)
{
#if USE_LSB_TAG
  jit_value_t nbits
    = jit_value_create_nint_constant (func, jit_type_int, INTTYPEBITS);
  jit_value_t val = jit_insn_shl (func, untagged_int, nbits);
  jit_value_t tag
    = jit_value_create_nint_constant (func, jit_type_void_ptr, Lisp_Int0);
  return jit_insn_add (func, val, tag);
#else /* USE_LSB_TAG */
  jit_value_t mask
    = jit_value_create_nint_constant (func, jit_type_void_ptr, INTMASK);
  jit_value_t tag
    = jit_value_create_nint_constant (func, jit_type_void_ptr,
				      ((EMACS_INT) Lisp_Int0) << VALBITS);
  jit_value_t val = jit_insn_and (func, untagged_int, mask);
  return jit_insn_add (func, val, tag);
#endif /* not USE_LSB_TAG */
}

static jit_value_t
compile_current_thread (jit_function_t func)
{
  jit_value_t thread_ptr = CONSTANT (func, &current_thread);
  return jit_insn_load_relative (func, thread_ptr, 0, jit_type_void_ptr);
}

static jit_value_t
compile_current_buffer (jit_function_t func)
{
  jit_value_t current_thread = compile_current_thread (func);
  return jit_insn_load_relative (func, current_thread,
				 offsetof (struct thread_state,
					   m_current_buffer),
				 jit_type_void_ptr);
}

static jit_value_t
compile_buffer_int (jit_function_t func, off_t offset)
{
  jit_value_t current_buffer_val = compile_current_buffer (func);
  jit_value_t value
    = jit_insn_load_relative (func, current_buffer_val, offset,
			      ptrdiff_t_type);

  return compile_make_natnum (func, value);
}

static jit_value_t
compare_buffer_ints (jit_function_t func, off_t off1, off_t off2)
{
  jit_value_t current_buffer_val = compile_current_buffer (func);
  jit_value_t value1
    = jit_insn_load_relative (func, current_buffer_val, off1,
			      ptrdiff_t_type);
  jit_value_t value2
    = jit_insn_load_relative (func, current_buffer_val, off2,
			      ptrdiff_t_type);
  return jit_insn_eq (func, value1, value2);
}

static void
car_or_cdr (jit_function_t func, jit_value_t val, off_t offset,
	    jit_label_t *next_insn, bool safe,
	    bool *called_wtype, jit_label_t *wtype_label,
	    jit_value_t wtype_arg)
{
  jit_value_t tem;
  jit_label_t not_a_cons = jit_label_undefined;

  jit_value_t is_cons = compare_type (func, val, Lisp_Cons);
  jit_insn_branch_if_not (func, is_cons, &not_a_cons);

  /* Is a cons.  */
  tem = untag (func, val, Lisp_Cons);
  tem = jit_insn_load_relative (func, tem, offset,
				jit_type_void_ptr);
  jit_insn_store (func, val, tem);
  jit_insn_branch (func, next_insn);

  jit_insn_label (func, &not_a_cons);
  if (safe)
    {
      /* Not a cons, so just use nil.  */
      jit_value_t nilval = CONSTANT (func, Qnil);
      jit_insn_store (func, val, nilval);
    }
  else
    {
      /* Check if it is nil. */
      tem = eq_nil (func, val);
      /* If it is nil, VAL is already correct and we can carry on.  */
      jit_insn_branch_if (func, tem, next_insn);

      /* Wrong type.  */
      jit_insn_store (func, wtype_arg, val);
      jit_insn_branch (func, wtype_label);
      *called_wtype = true;
    }
}

static void
compile_wrong_type_argument (jit_function_t func, jit_label_t *label,
			     jit_value_t wtype_arg)
{
  jit_value_t args[2];

  args[0] = CONSTANT (func, Qlistp);
  args[1] = wtype_arg;

  jit_insn_label (func, label);
  jit_insn_call_native (func, "wrong_type_argument",
			(void *) wrong_type_argument,
			/* FIXME incorrect signature.  */
			binary_signature, args, 2,
			JIT_CALL_NORETURN);
}

static jit_value_t
compare_integerp (jit_function_t func, jit_value_t val, jit_value_t *type_out)
{
  jit_value_t type = get_type (func, val);
  if (type_out != NULL)
    *type_out = type;

  jit_value_t c1
    = jit_value_create_nint_constant (func, jit_type_void_ptr,
				      Lisp_Int0 | ~Lisp_Int1);
  jit_value_t c2
    = jit_value_create_nint_constant (func, jit_type_void_ptr,
				      Lisp_Int0);
  jit_value_t tem = jit_insn_and (func, type, c1);

  return jit_insn_eq (func, tem, c2);
}

static Lisp_Object
negate (Lisp_Object arg)
{
  return Fminus (1, &arg);
}

enum math_op
{
  SUB1,
  ADD1,
  NEGATE
};

static void
unary_intmath (jit_function_t func, jit_value_t val, enum math_op op,
	       jit_label_t *next_insn)
{
  jit_label_t not_an_int = jit_label_undefined;
  jit_value_t compare = compare_integerp (func, val, NULL);

  jit_insn_branch_if_not (func, compare, &not_an_int);

  /* Got an integer.  */
  jit_value_t result = to_int (func, val);
  jit_value_t tem;
  switch (op)
    {
    case SUB1:
      tem = jit_value_create_nint_constant (func, jit_type_sys_int, 1);
      result = jit_insn_sub (func, result, tem);
      break;

    case ADD1:
      tem = jit_value_create_nint_constant (func, jit_type_sys_int, 1);
      result = jit_insn_add (func, result, tem);
      break;

    case NEGATE:
      result = jit_insn_neg (func, result);
      break;

    default:
      emacs_abort ();
    }

  result = compile_make_number (func, result);
  jit_insn_store (func, val, result);
  jit_insn_branch (func, next_insn);

  jit_insn_label (func, &not_an_int);

  const char *name;
  void *callee;
  switch (op)
    {
    case SUB1:
      name = "Fsub1";
      callee = (void *) Fsub1;
      break;
    case ADD1:
      name = "Fadd1";
      callee = (void *) Fadd1;
      break;
    case NEGATE:
      name = "negate";
      callee = (void *) negate;
      break;
    default:
      emacs_abort ();
    }

  tem = jit_insn_call_native (func, name, (void *) callee,
			      unary_signature, &val, 1,
			      JIT_CALL_NOTHROW);
  jit_insn_store (func, val, tem);
}

/* Compile a "MANY"-style call to a specific function.  */
static jit_value_t
compile_callN (jit_function_t func, const char *name,
	       Lisp_Object (*callee) (ptrdiff_t, Lisp_Object *),
	       int howmany, jit_value_t scratch, jit_value_t *stack)
{
  jit_value_t args[2];

  args[0] = jit_value_create_nint_constant (func, ptrdiff_t_type, howmany);
  args[1] = scratch;

  int i;
  for (i = 0; i < howmany; ++i)
    jit_insn_store_relative (func, scratch, i * sizeof (Lisp_Object),
			     stack[i]);

  return jit_insn_call_native (func, name, (void *) callee,
			       callN_signature, args, 2, JIT_CALL_NOTHROW);
}

/* Try to compile a direct call to a bytecode function.  */
static bool
compile_bytecode_direct (jit_function_t func, int howmany,
			 int index, jit_value_t *stack,
			 Lisp_Object *vectorp, jit_value_t *result)
{
  struct Lisp_Vector *vec = XVECTOR (vectorp[index]);

  ptrdiff_t at = XINT (vec->contents[COMPILED_ARGLIST]);
  bool rest = (at & 128) != 0;
  ptrdiff_t mandatory = at & 127;
  ptrdiff_t nonrest = at >> 8;

  /* If we can't compile it using a fixed-argument convention,
     fall back.  FIXME it would be nice to relax this.  */
  if (rest || nonrest >= SUBR_MAX_ARGS)
    return false;

  /* If this call site looks like it will cause an exception,
     don't bother.  */
  if (howmany > nonrest || howmany < mandatory)
    return false;

  /* I couldn't think of a way where this could end up recursing
     into the current compilation, so this just eagerly compiles.
     Lazy compilation could be added with a bit more effort -- the
     main problem being what to do if it fails.  */
  if (vec->contents[COMPILED_JIT_CODE] == NULL)
    {
      Lisp_Object callee;
      XSETCOMPILED (callee, vec);
      /* On failure just fall back to Ffuncall.  */
      if (!emacs_jit_compile_nolock (callee))
	return false;
      eassert (vec->contents[COMPILED_JIT_CODE] != NULL);
    }

  jit_value_t args[SUBR_MAX_ARGS];

  /* Copy in the known arguments.  */
  for (int i = 0; i < howmany; ++i)
    args[i] = stack[i];

  /* If there are optional arguments, set them all to Qnil.  */
  if (howmany < nonrest)
    {
      jit_value_t qnil = CONSTANT (func, Qnil);
      for (int i = howmany; i < nonrest; ++i)
	args[i] = qnil;
    }

  struct subr_function *subr
    = (struct subr_function *) vec->contents[COMPILED_JIT_CODE];
  void *jfunc = (void *) subr->function.a0;

  *result = jit_insn_call_native (func, "direct call", jfunc,
				  subr_signature[nonrest], args, nonrest,
				  JIT_CALL_NOTHROW);
  return true;
}

/* Try to compile a direct call to a subr.  */
static bool
compile_subr_direct (jit_function_t func, int howmany,
		     struct Lisp_Subr *subr, jit_value_t *stack,
		     jit_value_t *result)
{
  if (subr->function.max_args == MANY || subr->function.max_args == UNEVALLED)
    return false;

  /* If we're going to throw, just bail.  */
  if (howmany < subr->function.min_args || howmany > subr->function.max_args)
    return false;

  int len = subr->function.max_args;
  jit_value_t args[SUBR_MAX_ARGS];

  /* Copy in the known arguments.  */
  for (int i = 0; i < howmany; ++i)
    args[i] = stack[i];

  /* If there are optional arguments, set them all to Qnil.  */
  if (howmany < len)
    {
      jit_value_t qnil = CONSTANT (func, Qnil);
      for (int i = howmany; i < len; ++i)
	args[i] = qnil;
    }

  void *jfunc = (void *) subr->function.function.a0;

  *result = jit_insn_call_native (func, subr->symbol_name, jfunc,
				  subr_signature[len], args, len,
				  JIT_CALL_NOTHROW);
  return true;
}

/* If ARG is a symbol whose function is a subr on
   jit-direct-call-subrs, return the subr; otherwise return NULL.  */
static struct Lisp_Subr *
is_direct_call (Lisp_Object arg)
{
  /* FIXME this can throw but we aren't allowed to throw at this
     point.  */
  arg = indirect_function (arg);
  if (!SUBRP (arg))
    return NULL;

  Lisp_Object found = Fmemq (arg, Vjit_direct_call_subrs);
  if (NILP (found))
    return NULL;
  return XSUBR (arg);
}

/* Compile a generic function call.  */
static jit_value_t
compile_funcall (jit_function_t func, int howmany,
		 jit_value_t scratch, jit_value_t *stack,
		 struct op_state *this_state, Lisp_Object *vectorp)
{
  int index = this_state->stack[this_state->stack_depth - howmany];
  if (index != -1)
    {
      Lisp_Object callee = vectorp[index];
      jit_value_t result;

      /* If we see a function call to bytecode -- not indirecting via
	 a symbol -- we can compile a direct call.  This computation
	 might look like it is missing a "- 1", but HOWMANY includes
	 the function itself.  */
      if (COMPILEDP (callee) && INTEGERP (AREF (callee, COMPILED_ARGLIST)))
	{
	  if (compile_bytecode_direct (func, howmany - 1, index, stack + 1,
				       vectorp, &result))
	    return result;
	  /* Fall through.  */
	}
      else if (SUBRP (callee))
	{
	  if (compile_subr_direct (func, howmany - 1, XSUBR (callee),
				   stack + 1, &result))
	    return result;
	  /* Fall through.  */
	}
      else if (SYMBOLP (callee))
	{
	  struct Lisp_Subr *subr = is_direct_call (callee);
	  if (subr
	      && compile_subr_direct (func, howmany - 1, subr,
				      stack + 1, &result))
	    return result;
	  /* Fall through.  */
	}
    }

  /* Just go via Ffuncall.  */
  return compile_callN (func, "Ffuncall", Ffuncall, howmany, scratch, stack);
}

static jit_value_t
compile_next_handlerlist (jit_function_t func)
{
  jit_value_t current_thread = compile_current_thread (func);
  jit_value_t hlist_ptr
    = jit_insn_load_relative (func, current_thread,
			      offsetof (struct thread_state,
					m_handlerlist),
			      jit_type_void_ptr);
  jit_value_t next
    = jit_insn_load_relative (func, hlist_ptr,
			      offsetof (struct handler, next),
			      jit_type_void_ptr);
  jit_insn_store_relative (func, current_thread,
			   offsetof (struct thread_state,
				     m_handlerlist),
			   next);

  return hlist_ptr;
}

#define COMPILE_CALLN(FUNC, N)						\
  do {									\
    jit_value_t result;							\
    DISCARD (N);							\
    result = compile_callN (func, # FUNC, FUNC,				\
			    N, scratch, &stack[stack_pointer + 1]);	\
    if (N > scratch_slots_needed)					\
      scratch_slots_needed = N;						\
    PUSH (result);							\
  } while (0)

static void
bcall0 (Lisp_Object f)
{
  if (FUNCTIONP (f))
    Ffuncall (1, &f);
}

static Lisp_Object
native_save_window_excursion (Lisp_Object v1)
{
  ptrdiff_t count1 = SPECPDL_INDEX ();
  record_unwind_protect (restore_window_configuration,
			 Fcurrent_window_configuration (Qnil));
  v1 = Fprogn (v1);
  unbind_to (count1, v1);
  return v1;
}

static Lisp_Object
native_temp_output_buffer_setup (Lisp_Object x)
{
  CHECK_STRING (x);
  temp_output_buffer_setup (SSDATA (x));
  return Vstandard_output;
}

static Lisp_Object
unbind_n (int val)
{
  return unbind_to (SPECPDL_INDEX () - val, Qnil);
}

static void
wrong_number_of_arguments (int mandatory, int nonrest, int nargs)
{
  Fsignal (Qwrong_number_of_arguments,
	   list2 (Fcons (make_number (mandatory), make_number (nonrest)),
		  make_number (nargs)));
}

#define COMPILE_BUFFER_INT(FIELD) \
  compile_buffer_int (func, offsetof (struct buffer, FIELD))

#define COMPARE_BUFFER_INTS(FIELD1, FIELD2)			\
  compare_buffer_ints (func,					\
		       offsetof (struct buffer, FIELD1),	\
		       offsetof (struct buffer, FIELD2))

static bool
find_hash_min_max_pc (struct Lisp_Hash_Table *htab,
		      EMACS_INT *min_pc, EMACS_INT *max_pc)
{
  for (ptrdiff_t i = 0; i < HASH_TABLE_SIZE (htab); ++i)
    if (!NILP (HASH_HASH (htab, i)))
      {
	Lisp_Object pc = HASH_VALUE (htab, i);
	if (!INTEGERP (pc))
	  return false;
	EMACS_INT pcval = XINT (pc);
	if (pcval < *min_pc)
	  *min_pc = pcval;
	if (pcval > *max_pc)
	  *max_pc = pcval;
      }

  ++*max_pc;
  return true;
}

static void
free_state (struct op_state *state, ptrdiff_t len)
{
  for (ptrdiff_t i = 0; i < len; ++i)
    xfree (state[i].stack);
  xfree (state);
}

static bool
merge_definitions (int *from, int *to, int depth)
{
  bool result = true;

  for (int i = 0; i < depth; ++i)
    if (from[i] != to[i])
      {
	result = false;
	to[i] = -1;
      }

  return result;
}

#define PREPASS_POP				\
  do {						\
    --stack_depth;				\
    eassert (stack_depth >= 0);			\
  } while (0)

#define PREPASS_PUSH(val)			\
  do {						\
    eassert (stack_depth < max_stack_depth);	\
    working_stack[stack_depth++] = val;		\
  } while (0)

struct stack_effect
{
  int pop;
  int push;
};

struct prepass_pc_list
{
  /* PC at which to (re-)start compilation.  */
  int pc;
  struct prepass_pc_list *next;
};

static bool
prepass_push_pc (struct op_state *states, ptrdiff_t target_pc,
		 int stack_depth, int *working_stack,
		 struct prepass_pc_list **head)
{
  states[target_pc].is_branch_target = true;

  if (!states[target_pc].stack_initialized)
    {
      states[target_pc].stack_initialized = true;
      states[target_pc].stack_depth = stack_depth;
      if (stack_depth > 0)
	{
	  states[target_pc].stack = xmalloc (stack_depth * sizeof (int));
	  memcpy (states[target_pc].stack, working_stack,
		  stack_depth * sizeof (int));
	}
    }
  else
    {
      if (stack_depth != states[target_pc].stack_depth)
	return false;

      if (merge_definitions (working_stack, states[target_pc].stack,
			     stack_depth))
	{
	  /* Don't need to re-check  */
	  return true;
	}
    }

  struct prepass_pc_list *new = xmalloc (sizeof (struct prepass_pc_list));
  new->pc = target_pc;
  new->next = *head;
  *head = new;
  return true;
}

static struct op_state *
compile_prepass (ptrdiff_t bytestr_length, unsigned char *bytestr_data,
		 int max_stack_depth,
		 Lisp_Object *vectorp, ptrdiff_t vector_size,
		 ptrdiff_t total_args)
{
#define DEFINE(name, opnum, npop, npush) \
  [opnum] = {npop, npush},

  static const struct stack_effect stack_effects[256] =
    {
     /* Invalid slots will have -2 to distinguish from valid
	slots.  */
     [0 ... (Bconstant - 1)] = {-2, -2},
     [Bconstant ... 255] = {-1, -1},

     BYTE_CODES
    };

#undef DEFINE

#define FAIL					\
  do {						\
    /* This can be handy when debugging.   */	\
    /* failure = __LINE__; */			\
    goto fail;					\
  } while (0)
  /* int failure = -1; */

  int stack_depth = 0;
  int *working_stack = xmalloc ((max_stack_depth + 1) * sizeof (int));
  for (ptrdiff_t i = 0; i < max_stack_depth + 1; ++i)
    working_stack[i] = -1;

  struct prepass_pc_list *pc_list = NULL;
  struct op_state *result = NULL;
  struct op_state *states
    = (struct op_state *) xzalloc (bytestr_length
				   * sizeof (struct op_state));

  /* Push arguments.  */
  for (ptrdiff_t i = 0; i < total_args; ++i)
    PREPASS_PUSH (-1);

  ptrdiff_t pc = 0;
  for (;;)
    {
      if (pc == bytestr_length)
	{
	  /* Falling off the end would be bad.  */
	  FAIL;
	}
      else if (pc != -1)
	{
	  states[pc].is_fallthrough_target = true;
	  if (states[pc].stack_initialized)
	    {
	      if (stack_depth != states[pc].stack_depth)
		{
		  /* Inconsistency.  */
		  FAIL;
		}

	      /* We've already processed this code, but now we're
		 falling through.  */
	      if (merge_definitions (working_stack, states[pc].stack,
				     stack_depth))
		{
		  /* This means merging detected no changes, so resume
		     work at some other PC.  */
		  pc = -1;
		}
	    }
	}

      /* If we don't have a PC currently, pop a new one from the list
	 and work there.  */
      while (pc == -1 && pc_list != NULL)
	{
	  struct prepass_pc_list *next;

	  pc = pc_list->pc;
	  next = pc_list->next;
	  xfree (pc_list);
	  pc_list = next;

	  eassert (states[pc].stack_initialized);

	  stack_depth = states[pc].stack_depth;
	  if (stack_depth > 0)
	    memcpy (working_stack, states[pc].stack,
		    stack_depth * sizeof (int));

	  // this might be less than perfectly efficient
	  // suppose a PC is on the list twice at the same time
	  // FIXME
	}

      if (pc == -1 && pc_list == NULL)
	{
	  /* No more blocks to examine.  */
	  break;
	}

      if (!states[pc].stack_initialized)
	{
	  states[pc].stack_initialized = true;
	  states[pc].stack_depth = stack_depth;
	  if (stack_depth > 0)
	    {
	      states[pc].stack = xmalloc (stack_depth * sizeof (int));
	      memcpy (states[pc].stack, working_stack,
		      stack_depth * sizeof (int));
	    }
	}

      states[pc].seen = true;

      int op = FETCH;

      const struct stack_effect *effect = &stack_effects[op];
      for (int i = 0; i < effect->pop; ++i)
	PREPASS_POP;
      for (int i = 0; i < effect->push; ++i)
	PREPASS_PUSH (-1);

      switch (op)
	{
	case Bvarref7:
	case Bvarset7:
	case Bvarbind7:
	case Bunbind7:
	  pc += 2;
	  break;

	case Bvarref6:
	case Bvarset6:
	case Bvarbind6:
	case Bunbind6:
	  ++pc;
	  break;

	case Bdup:
	  op = 0;
	  goto do_stack_ref;

	case Bstack_ref7:
	  op = FETCH2;
	  goto do_stack_ref;

	case Bstack_ref6:
	  op = FETCH;
	  goto do_stack_ref;

	case Bstack_ref1:
	case Bstack_ref2:
	case Bstack_ref3:
	case Bstack_ref4:
	case Bstack_ref5:
	  op -= Bstack_ref;

	do_stack_ref:
	  {
	    int val = working_stack[stack_depth - 1 - op];
	    PREPASS_PUSH (val);
	  }
	  break;

	case Bcall7:
	  op = 1 + FETCH2;
	  goto do_N;

	case Bcall6:
	  op = 1 + FETCH;
	  goto do_N;

	case BlistN:
	case BconcatN:
	case BinsertN:
	  op = FETCH;
	do_N:
	  for (int i = 0; i < op; ++i)
	    PREPASS_POP;
	  PREPASS_PUSH (-1);
	  break;

	case BdiscardN:
	  {
	    op = FETCH;
	    if (op & 0x80)
	      {
		op &= 0X7F;
		working_stack[stack_depth - 1 - op]
		  = working_stack[stack_depth - 1];
	      }
	    for (int i = 0; i < op; ++i)
	      PREPASS_POP;
	  }
	  break;

	case Bstack_set:
	  op = FETCH;
	  goto do_stack_set;

	case Bstack_set2:
	  op = FETCH2;
	do_stack_set:
	  {
	    working_stack[stack_depth - 1 - op]
	      = working_stack[stack_depth - 1];
	    PREPASS_POP;
	  }
	  break;

	case Bgoto:
	  op = FETCH2;
	  if (!prepass_push_pc (states, op, stack_depth, working_stack,
				&pc_list))
	    FAIL;
	  pc = -1;
	  break;

	case Bgotoifnil:
	case Bgotoifnonnil:
	  {
	    op = FETCH2;
	    if (!prepass_push_pc (states, op, stack_depth, working_stack,
				  &pc_list))
	      FAIL;
	    break;
	  }

	case Bgotoifnilelsepop:
	case Bgotoifnonnilelsepop:
	  {
	    op = FETCH2;
	    if (!prepass_push_pc (states, op, stack_depth, working_stack,
				  &pc_list))
	      FAIL;
	    PREPASS_POP;
	    break;
	  }
	  break;

	case BRgoto:
	  {
	    op = FETCH - 128;
	    op += pc;
	    if (!prepass_push_pc (states, op, stack_depth, working_stack,
				  &pc_list))
	      FAIL;
	    pc = -1;
	    break;
	  }

	case BRgotoifnil:
	case BRgotoifnonnil:
	  {
	    op = FETCH - 128;
	    op += pc;
	    if (!prepass_push_pc (states, op, stack_depth, working_stack,
				  &pc_list))
	      FAIL;
	    break;
	  }

	case BRgotoifnilelsepop:
	case BRgotoifnonnilelsepop:
	  {
	    op = FETCH - 128;
	    op += pc;
	    if (!prepass_push_pc (states, op, stack_depth, working_stack,
				  &pc_list))
	      FAIL;
	    PREPASS_POP;
	    break;
	  }

	case Breturn:
	  pc = -1;
	  break;

	case Bpushcatch:	/* New in 24.4.  */
	case Bpushconditioncase: /* New in 24.4.  */
	  op = FETCH2;
	  PREPASS_POP;
	  /* The value pushed on exception comes from nowhere.  */
	  PREPASS_PUSH (-1);
	  if (!prepass_push_pc (states, op, stack_depth, working_stack,
				&pc_list))
	    FAIL;
	  PREPASS_POP;
	  break;

	case Bswitch:
	  {
	    if (stack_depth == 0 || working_stack[stack_depth - 1] == -1)
	      {
		/* We require a constant for Bswitch.  */
		FAIL;
	      }

            Lisp_Object table = vectorp[working_stack[stack_depth - 1]];
	    if (!HASH_TABLE_P (table))
	      FAIL;
	    struct Lisp_Hash_Table *h = XHASH_TABLE (table);
	    /* These pops have to come after we examine the stack.  */
	    PREPASS_POP;
	    PREPASS_POP;
	    for (ptrdiff_t i = 0; i < HASH_TABLE_SIZE (h); ++i)
	      {
		if (!NILP (HASH_HASH (h, i)))
		  {
		    Lisp_Object target_pc = HASH_VALUE (h, i);
		    if (!INTEGERP (target_pc))
		      FAIL;
		    EMACS_INT pcval = XINT (target_pc);
		    if (!prepass_push_pc (states, pcval, stack_depth,
					  working_stack, &pc_list))
		      FAIL;
		  }
	      }
	  }
	  break;

	case Bconstant2:
	  op = FETCH2;
	  PREPASS_PUSH (op);
	  break;

	case Bconstant:
	default:
	  /* An invalid code means compilation failure.  */
	  if (effect->pop == -2)
	    FAIL;

	  if (op >= Bconstant && op < Bconstant + vector_size)
	    {
	      op -= Bconstant;
	      PREPASS_PUSH (op);
	    }
	  else
	    {
	      /* Anything requiring special treatment must be handled
		 by some other case in this switch.  */
	      eassert (effect->pop != -1);
	    }
	  break;
	}
    }

  result = states;

 fail:

  if (result == NULL)
    {
      /* Clean up, but note that the stack slot freeing will be
	 handled by the caller.  */
      while (pc_list != NULL)
	{
	  struct prepass_pc_list *next = pc_list->next;
	  xfree (pc_list);
	  pc_list = next;
	}
      free_state (states, bytestr_length);
    }
  else
    eassert (pc_list == NULL);
  xfree (working_stack);

  return result;
}

#define CHECK_BRANCH(Newpc)					\
  do {								\
    eassert (states[Newpc].stack_depth == stack_pointer + 1);	\
  } while (0)

static struct subr_function *
compile (ptrdiff_t bytestr_length, unsigned char *bytestr_data,
	 EMACS_INT max_stack_depth, Lisp_Object *vectorp,
	 ptrdiff_t vector_size, Lisp_Object args_template)
{
  int type;

  /* Note that any error before this is attached to the function must
     free this object.  */
  /* struct subr_function *result = xmalloc (sizeof (struct subr_function)); */
  /* result->min_args = 0; */
  /* result->max_args = MANY; */

  jit_type_t function_signature = compiled_signature;

  bool rest = false;
  ptrdiff_t mandatory = 0;
  ptrdiff_t nonrest = 0;

  bool parse_args = true;
  if (INTEGERP (args_template))
    {
      ptrdiff_t at = XINT (args_template);
      rest = (at & 128) != 0;
      mandatory = at & 127;
      nonrest = at >> 8;

      /* Always set this correctly so that funcall_subr will do some
	 checking for us.  */
      result->min_args = mandatory;

      if (!rest && nonrest < SUBR_MAX_ARGS)
	{
	  result->max_args = nonrest;
	  function_signature = subr_signature[nonrest];
	  parse_args = false;
	}
    }

  struct op_state *states = compile_prepass (bytestr_length, bytestr_data,
					     max_stack_depth,
					     vectorp, vector_size,
					     nonrest + (int) rest);
  if (states == NULL)
    {
      xfree (result);
      return NULL;
    }

  jit_function_t func = jit_function_create (emacs_jit_context,
					     function_signature);
  ptrdiff_t pc = 0;
  jit_value_t *stack = (jit_value_t *) xmalloc (max_stack_depth
						* sizeof (jit_value_t));
  int stack_pointer = -1;
  /* Temporary array used only for switches.  */
  jit_label_t *sw_labels = (jit_label_t *) xmalloc (bytestr_length
						    * sizeof (jit_label_t));
  jit_value_t n_args, arg_vec;

  /* On failure this will also free RESULT.  */
  jit_function_set_meta (func, 0, result, xfree, 0);

  for (int i = 0; i < bytestr_length; ++i)
    {
      states[i].label = jit_label_undefined;
      sw_labels[i] = jit_label_undefined;
    }

  for (int i = 0; i < max_stack_depth; ++i)
    stack[i] = jit_value_create (func, jit_type_void_ptr);

  /* This is a placeholder; once we know how much space we'll need, we
     will allocate it and move it into place at the start of the
     function.  */
  jit_value_t scratch = jit_value_create (func, jit_type_void_ptr);
  int scratch_slots_needed = 0;

  /* State needed if we need to emit a call to
     wrong_type_argument.  */
  bool called_wtype = false;
  jit_label_t wtype_label = jit_label_undefined;
  jit_value_t wtype_arg = jit_value_create (func, jit_type_void_ptr);

  jit_label_t argfail = jit_label_undefined;
  bool need_argfail = false;
  jit_value_t mandatory_val, nonrest_val;

  if (!parse_args)
    {
      /* We can emit a function that doesn't need to manually decipher
	 its arguments.  */
      for (ptrdiff_t i = 0; i < nonrest; ++i)
	PUSH (jit_value_get_param (func, i));
    }
  else
    {
      /* Prologue.  */
      n_args = jit_value_get_param (func, 0);
      arg_vec = jit_value_get_param (func, 1);

      if (INTEGERP (args_template))
	{
	  mandatory_val
	    = jit_value_create_long_constant (func, ptrdiff_t_type, mandatory);
	  nonrest_val
	    = jit_value_create_nint_constant (func, ptrdiff_t_type, nonrest);

	  /* If there are no rest arguments and we have more than the
	     maximum, error.  Note that funcall_subr ensures that, no
	     matter what, we'll never see fewer than the minimum
	     number of arguments.  */
	  if (!rest)
	    {
	      jit_value_t compare = jit_insn_gt (func, n_args, nonrest_val);
	      jit_insn_branch_if (func, compare, &argfail);
	      need_argfail = true;
	    }

	  /* Load mandatory arguments.  */
	  for (ptrdiff_t i = 0; i < mandatory; ++i)
	    {
	      jit_value_t loaded
		= jit_insn_load_relative (func, arg_vec, i * sizeof (Lisp_Object),
					  jit_type_void_ptr);
	      jit_insn_store (func, stack[i], loaded);
	    }

	  /* &optional arguments are a bit weirder since we can't refer to
	     the appropriate stack slot by index at runtime.  */
	  if (nonrest > mandatory)
	    {
	      jit_value_t qnil = CONSTANT (func, Qnil);
	      jit_label_t *opt_labels
		= (jit_label_t *) xmalloc ((nonrest - mandatory)
					   * sizeof (jit_label_t));
	      jit_label_t opts_done = jit_label_undefined;

	      for (ptrdiff_t i = mandatory; i < nonrest; ++i)
		{
		  opt_labels[i - mandatory] = jit_label_undefined;

		  jit_value_t this_arg
		    = jit_value_create_nint_constant (func, jit_type_sys_int, i);
		  jit_value_t cmp = jit_insn_le (func, n_args, this_arg);
		  /* If this argument wasn't found, then neither are the
		     subsequent ones; so branch into the correct spot in a
		     series of loads of Qnil.  */
		  jit_insn_branch_if (func, cmp, &opt_labels[i - mandatory]);

		  jit_value_t loaded
		    = jit_insn_load_relative (func, arg_vec,
					      i * sizeof (Lisp_Object),
					      jit_type_void_ptr);
		  jit_insn_store (func, stack[i], loaded);
		}

	      jit_insn_branch (func, &opts_done);

	      for (ptrdiff_t i = mandatory; i < nonrest; ++i)
		{
		  jit_insn_label (func, &opt_labels[i - mandatory]);
		  jit_insn_store (func, stack[i], qnil);
		}

	      jit_insn_label (func, &opts_done);
	      xfree (opt_labels);
	    }

	  stack_pointer = nonrest - 1;

	  /* Now handle rest arguments, if any.  */
	  if (rest)
	    {
	      jit_label_t no_rest = jit_label_undefined;
	      jit_value_t cmp = jit_insn_lt (func, nonrest_val, n_args);
	      jit_insn_branch_if_not (func, cmp, &no_rest);

	      jit_value_t vec_addr
		= jit_insn_load_elem_address (func, arg_vec, nonrest_val,
					      jit_type_void_ptr);
	      jit_value_t new_args
		= jit_insn_sub (func, n_args, nonrest_val);

	      jit_value_t args[2] = { new_args, vec_addr };
	      jit_value_t listval
		= jit_insn_call_native (func, "list", (void *) Flist,
					callN_signature,
					args, 2, JIT_CALL_NOTHROW);
	      PUSH (listval);
	      jit_insn_branch (func, &states[0].label);

	      /* Since we emitted a branch.  */
	      --stack_pointer;
	      jit_insn_label (func, &no_rest);
	      jit_value_t qnil = CONSTANT (func, Qnil);
	      PUSH (qnil);
	    }

	  /* Fall through to the main body.  */
	}
    }

  /* We can do the compilation in bytecode order because the prepass
     has already computed the information we need in order to do
     so.  */
  while (pc < bytestr_length)
    {
      /* Sometimes the byte compiler can emit dead code.  We can just
	 skip that.  E.g., disassemble "set-locale-environment" and
	 look for the redundant "goto" after the "switch".  */
      while (pc < bytestr_length && !states[pc].seen)
	++pc;
      if (pc == bytestr_length)
	break;

      /* Always emit a label because sometimes an instruction will
	 have to branch to the next instruction -- but this isn't
	 explicitly accounted for by "is_branch_target".  */
      jit_insn_label (func, &states[pc].label);

      eassert (states[pc].stack_initialized);
      stack_pointer = states[pc].stack_depth - 1;

      ptrdiff_t orig_pc = pc;
      int op = FETCH;
      switch (op)
	{
	case Bvarref7:
	  op = FETCH2;
	  goto varref;

	case Bvarref:
	case Bvarref1:
	case Bvarref2:
	case Bvarref3:
	case Bvarref4:
	case Bvarref5:
	  op -= Bvarref;
	  goto varref;

	case Bvarref6:
	  op = FETCH;
	varref:
	  {
	    jit_value_t sym, result;

	    sym = CONSTANT (func, vectorp[op]);
	    result = jit_insn_call_native (func, "symbol-value",
					   (void *) Fsymbol_value,
					   unary_signature, &sym, 1,
					   JIT_CALL_NOTHROW);
	    PUSH (result);
	    break;
	  }

	case Bcar:
	  car_or_cdr (func, TOP, offsetof (struct Lisp_Cons, u.s.car),
		      &states[pc].label, false,
		      &called_wtype, &wtype_label, wtype_arg);
	  break;

	case Beq:
	  {
	    jit_value_t v1 = POP;
	    jit_value_t compare = jit_insn_eq (func, v1, TOP);
	    emit_qnil_or_qt (func, compare, TOP, &states[pc].label);
	    break;
	  }

	case Bmemq:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "memq", Fmemq, TOP, v1);
	    break;
	  }

	case Bcdr:
	  car_or_cdr (func, TOP, offsetof (struct Lisp_Cons, u.s.u.cdr),
		      &states[pc].label, false,
		      &called_wtype, &wtype_label, wtype_arg);
	  break;

	case Bvarset:
	case Bvarset1:
	case Bvarset2:
	case Bvarset3:
	case Bvarset4:
	case Bvarset5:
	  op -= Bvarset;
	  goto varset;

	case Bvarset7:
	  op = FETCH2;
	  goto varset;

	case Bvarset6:
	  op = FETCH;
	varset:
	  {
	    jit_value_t args[4];

	    args[0] = CONSTANT (func, vectorp[op]);
	    args[1] = POP;
	    args[2] = CONSTANT (func, Qnil);
	    args[3] = jit_value_create_nint_constant (func, jit_type_sys_int,
						      SET_INTERNAL_SET);

	    jit_insn_call_native (func, "set_internal", (void *) set_internal,
				  set_internal_signature, args, 4,
				  JIT_CALL_NOTHROW);
	  }
	  break;

	case Bdup:
	  {
	    jit_value_t v1 = TOP;
	    PUSH (v1);
	    break;
	  }

	/* ------------------ */

	case Bvarbind6:
	  op = FETCH;
	  goto varbind;

	case Bvarbind7:
	  op = FETCH2;
	  goto varbind;

	case Bvarbind:
	case Bvarbind1:
	case Bvarbind2:
	case Bvarbind3:
	case Bvarbind4:
	case Bvarbind5:
	  op -= Bvarbind;
	varbind:
	  {
	    jit_value_t vals[2];

	    vals[0] = CONSTANT (func, vectorp[op]);
	    vals[1] = POP;

	    jit_insn_call_native (func, "specbind", (void *) specbind,
				  specbind_signature, vals, 2,
				  JIT_CALL_NOTHROW);
	    break;
	  }

	case Bcall6:
	  op = FETCH;
	  goto docall;

	case Bcall7:
	  op = FETCH2;
	  goto docall;

	case Bcall:
	case Bcall1:
	case Bcall2:
	case Bcall3:
	case Bcall4:
	case Bcall5:
	  op -= Bcall;
	docall:
	  {
	    jit_value_t result;

	    DISCARD (op + 1);
	    result = compile_funcall (func, op + 1,
				      scratch, &stack[stack_pointer + 1],
				      &states[orig_pc], vectorp);
	    if (op + 1 > scratch_slots_needed)
	      scratch_slots_needed = op + 1;
	    PUSH (result);

	    break;
	  }

	case Bunbind6:
	  op = FETCH;
	  goto dounbind;

	case Bunbind7:
	  op = FETCH2;
	  goto dounbind;

	case Bunbind:
	case Bunbind1:
	case Bunbind2:
	case Bunbind3:
	case Bunbind4:
	case Bunbind5:
	  op -= Bunbind;
	dounbind:
	  {
	    jit_value_t val = jit_value_create_nint_constant (func,
							      jit_type_sys_int,
							      op);
	    jit_insn_call_native (func, "unbind_n", (void *) unbind_n,
				  unbind_n_signature, &val, 1,
				  JIT_CALL_NOTHROW);
	  }
	  break;

	case Bgoto:
	  op = FETCH2;
	  CHECK_BRANCH (op);
	  jit_insn_branch (func, &states[op].label);
	  orig_pc = -1;
	  break;

	case Bgotoifnil:
	  {
	    jit_value_t v1 = POP;
	    jit_value_t compare = eq_nil (func, v1);
	    op = FETCH2;
	    CHECK_BRANCH (op);
	    jit_insn_branch_if (func, compare, &states[op].label);
	    break;
	  }

	case Bgotoifnonnil:
	  {
	    jit_value_t val = POP;
	    jit_value_t compare = eq_nil (func, val);
	    op = FETCH2;
	    CHECK_BRANCH (op);
	    jit_insn_branch_if_not (func, compare, &states[op].label);
	    break;
	  }

	case Bgotoifnilelsepop:
	  {
	    jit_value_t v1 = TOP;
	    jit_value_t compare = eq_nil (func, v1);
	    op = FETCH2;
	    CHECK_BRANCH (op);
	    jit_insn_branch_if (func, compare, &states[op].label);
	    DISCARD (1);
	    break;
	  }
	  break;

	case Bgotoifnonnilelsepop:
	  {
	    jit_value_t v1 = TOP;
	    jit_value_t compare = eq_nil (func, v1);
	    op = FETCH2;
	    CHECK_BRANCH (op);
	    jit_insn_branch_if_not (func, compare, &states[op].label);
	    DISCARD (1);
	    break;
	  }
	  break;

	case BRgoto:
	  {
	    op = FETCH - 128;
	    op += pc;
	    CHECK_BRANCH (op);
	    jit_insn_branch (func, &states[op].label);
	    orig_pc = -1;
	    break;
	  }

	case BRgotoifnil:
	  {
	    jit_value_t v1 = POP;
	    jit_value_t compare = eq_nil (func, v1);
	    op = FETCH - 128;
	    op += pc;
	    CHECK_BRANCH (op);
	    jit_insn_branch_if (func, compare, &states[op].label);
	    break;
	  }

	case BRgotoifnonnil:
	  {
	    jit_value_t v1 = POP;
	    jit_value_t compare = eq_nil (func, v1);
	    op = FETCH - 128;
	    op += pc;
	    CHECK_BRANCH (op);
	    jit_insn_branch_if_not (func, compare, &states[op].label);
	    break;
	  }

	case BRgotoifnilelsepop:
	  {
	    jit_value_t v1 = TOP;
	    jit_value_t compare = eq_nil (func, v1);
	    op = FETCH - 128;
	    op += pc;
	    CHECK_BRANCH (op);
	    jit_insn_branch_if (func, compare, &states[op].label);
	    DISCARD (1);
	    break;
	  }

	case BRgotoifnonnilelsepop:
	  {
	    jit_value_t v1 = TOP;
	    jit_value_t compare = eq_nil (func, v1);
	    op = FETCH - 128;
	    op += pc;
	    CHECK_BRANCH (op);
	    jit_insn_branch_if_not (func, compare, &states[op].label);
	    DISCARD (1);
	    break;
	  }

	case Breturn:
	  jit_insn_return (func, TOP);
	  orig_pc = -1;
	  break;

	case Bdiscard:
	  DISCARD (1);
	  break;

	case Bsave_excursion:
	  {
	    jit_value_t vals[2];

	    vals[0] = CONSTANT (func, save_excursion_restore);
	    vals[1] = jit_insn_call_native (func, "save_excursion_save",
					    (void *) save_excursion_save,
					    save_excursion_save_signature,
					    NULL, 0, JIT_CALL_NOTHROW);
	    jit_insn_call_native (func, "record_unwind_protect",
				  (void *) record_unwind_protect,
				  record_unwind_protect_signature,
				  vals, 2, JIT_CALL_NOTHROW);
	    break;
	  }

	case Bsave_current_buffer: /* Obsolete since ??.  */
	case Bsave_current_buffer_1:
	  jit_insn_call_native (func, "record_unwind_current_buffer",
				(void *) record_unwind_current_buffer,
				void_void_signature,
				NULL, 0, JIT_CALL_NOTHROW);
	  break;

	case Bsave_window_excursion: /* Obsolete since 24.1.  */
	  {
	    compile_unary (func, "save-window-excursion",
			   native_save_window_excursion, TOP);
	    break;
	  }

	case Bsave_restriction:
	  {
	    jit_value_t vals[2];

	    vals[0] = CONSTANT (func, save_restriction_restore);
	    vals[1] = jit_insn_call_native (func, "save_restriction_save",
					    (void *) save_restriction_save,
					    void_void_signature,
					    NULL, 0, JIT_CALL_NOTHROW);
	    jit_insn_call_native (func, "record_unwind_protect",
				  (void *) record_unwind_protect,
				  record_unwind_protect_signature,
				  vals, 2, JIT_CALL_NOTHROW);
	    break;
	  }

	case Bcatch:		/* Obsolete since 24.4.  */
	  {
	    jit_value_t args[3];

	    args[1] = CONSTANT (func, eval_sub);
	    args[2] = POP;
	    args[0] = POP;

	    /* FIXME this lies about the signature.  */
	    jit_value_t result = jit_insn_call_native (func, "internal_catch",
						       internal_catch,
						       ternary_signature,
						       args, 3,
						       JIT_CALL_NOTHROW);
	    PUSH (result);
	    break;
	  }

	case Bpushcatch:	/* New in 24.4.  */
	  type = CATCHER;
	  goto pushhandler;
	case Bpushconditioncase: /* New in 24.4.  */
	  type = CONDITION_CASE;
	pushhandler:
	  {
	    jit_value_t args[2];
	    jit_value_t handler, cond;
	    int handler_pc = FETCH2;

	    args[0] = POP;
	    args[1] = jit_value_create_nint_constant (func, jit_type_sys_int,
						      type);

	    handler = jit_insn_call_native (func, "push_handler", push_handler,
					    push_handler_signature,
					    args, 2, JIT_CALL_NOTHROW);
	    jit_value_t jmp
	      = jit_insn_add_relative (func, handler,
				       offsetof (struct handler, jmp));

	    /* FIXME probably should be using the same as the rest of
	       emacs.  */
	    cond = jit_insn_call_native (func, "sys_setjmp", setjmp,
					 setjmp_signature,
					 &jmp, 1, JIT_CALL_NOTHROW);
	    CHECK_BRANCH (pc);
	    jit_insn_branch_if_not (func, cond, &states[pc].label);

	    /* Something threw to here.  */
	    jit_value_t hlist = compile_next_handlerlist (func);

	    jit_value_t val
	      = jit_insn_load_relative (func, hlist,
					offsetof (struct handler, val),
					jit_type_void_ptr);

	    PUSH (val);
	    CHECK_BRANCH (handler_pc);
	    jit_insn_branch (func, &states[handler_pc].label);
	    orig_pc = -1;
	    break;
	  }

	case Bpophandler:	/* New in 24.4.  */
	  {
	    compile_next_handlerlist (func);
	    break;
	  }

	case Bunwind_protect:	/* FIXME: avoid closure for lexbind.  */
	  {
	    jit_value_t args[2];

	    args[0] = CONSTANT (func, bcall0);
	    args[1] = POP;
	    jit_insn_call_native (func, "record_unwind_protect",
				  (void *) record_unwind_protect,
				  record_unwind_protect_signature,
				  args, 2, JIT_CALL_NOTHROW);
	    break;
	  }

	case Bcondition_case:		/* Obsolete since 24.4.  */
	  {
	    jit_value_t handlers = POP, body = POP;
	    compile_ternary (func, "condition-case",
			     internal_lisp_condition_case, TOP,
			     body, handlers);
	    break;
	  }

	case Btemp_output_buffer_setup: /* Obsolete since 24.1.  */
	  {
	    compile_unary (func, "temp-output-buffer-setup",
			   native_temp_output_buffer_setup, TOP);
	    break;
	  }

	case Btemp_output_buffer_show: /* Obsolete since 24.1.  */
	  {
	    jit_value_t v1 = POP;
	    jit_value_t v2 = TOP;
	    jit_value_t tem;

	    jit_insn_call_native (func, "temp_output_buffer_show",
				  (void *) temp_output_buffer_show,
				  temp_output_buffer_show_signature,
				  &v2, 1, JIT_CALL_NOTHROW);
	    jit_insn_store (func, TOP, v1);

	    tem = jit_value_create_nint_constant (func, jit_type_sys_int, 1);
	    jit_insn_call_native (func, "unbind_n", (void *) unbind_n,
				  unbind_n_signature, &tem, 1,
				  JIT_CALL_NOTHROW);
	    break;
	  }

	case Bnth:
	  {
	    jit_value_t v2 = POP;
	    compile_binary (func, "nth", Fnth, TOP, v2);
	    break;
	  }

	case Bsymbolp:
	  {
	    jit_value_t compare = compare_type (func, TOP, Lisp_Symbol);
	    emit_qnil_or_qt (func, compare, TOP, &states[pc].label);
	    break;
	  }

	case Bconsp:
	  {
	    jit_value_t compare = compare_type (func, TOP, Lisp_Cons);
	    emit_qnil_or_qt (func, compare, TOP, &states[pc].label);
	    break;
	  }

	case Bstringp:
	  {
	    jit_value_t compare = compare_type (func, TOP, Lisp_String);
	    emit_qnil_or_qt (func, compare, TOP, &states[pc].label);
	    break;
	  }

	case Blistp:
	  {
	    jit_value_t tem, nilval;
	    jit_label_t not_a_cons = jit_label_undefined;
	    jit_label_t not_nil = jit_label_undefined;

	    jit_value_t is_cons = compare_type (func, TOP, Lisp_Cons);
	    jit_insn_branch_if_not (func, is_cons, &not_a_cons);

	    /* Is a cons.  */
	    tem = CONSTANT (func, Qt);
	    jit_insn_store (func, TOP, tem);
	    jit_insn_branch (func, &states[pc].label);

	    jit_insn_label (func, &not_a_cons);

	    nilval = CONSTANT (func, Qnil);
	    tem = jit_insn_eq (func, TOP, nilval);

	    jit_insn_branch_if_not (func, tem, &not_nil);

	    /* Is nil.  */
	    tem = CONSTANT (func, Qt);
	    jit_insn_store (func, TOP, tem);
	    jit_insn_branch (func, &states[pc].label);

	    jit_insn_label (func, &not_nil);
	    jit_insn_store (func, TOP, nilval);
	  }
	  break;

	case Bnot:
	  {
	    jit_value_t compare = eq_nil (func, TOP);
	    emit_qnil_or_qt (func, compare, TOP, &states[pc].label);
	    break;
	  }

	case Bcons:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "cons", Fcons, TOP, v1);
	    break;
	  }

	case BlistN:
	  op = FETCH;
	  goto make_list;

	case Blist1:
	case Blist2:
	case Blist3:
	case Blist4:
	  op = op + 1 - Blist1;
	make_list:
	  {
	    jit_value_t args[2];

	    int i;
	    args[1] = CONSTANT (func, Qnil);

	    for (i = 0; i < op; ++i)
	      {
		args[0] = POP;
		args[1] = jit_insn_call_native (func, "cons", (void *) Fcons,
						binary_signature, args, 2,
						JIT_CALL_NOTHROW);
	      }

	    PUSH (args[1]);
	    break;
	  }

	case Blength:
	  compile_unary (func, "length", Flength, TOP);
	  break;

	case Baref:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "aref", Faref, TOP, v1);
	    break;
	  }

	case Baset:
	  {
	    jit_value_t v2 = POP, v1 = POP;
	    compile_ternary (func, "aset", Faset, TOP, v1, v2);
	    break;
	  }

	case Bsymbol_value:
	  compile_unary (func, "symbol-value", Fsymbol_value, TOP);
	  break;

	case Bsymbol_function:
	  compile_unary (func, "symbol-function", Fsymbol_function, TOP);
	  break;

	case Bset:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "set", Fset, TOP, v1);
	    break;
	  }

	case Bfset:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "fset", Ffset, TOP, v1);
	    break;
	  }

	case Bget:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "get", Fget, TOP, v1);
	    break;
	  }

	case Bsubstring:
	  {
	    jit_value_t v2 = POP, v1 = POP;
	    compile_ternary (func, "substring", Fsubstring, TOP, v1, v2);
	    break;
	  }

	case Bconcat2:
	  {
	    COMPILE_CALLN (Fconcat, 2);
	    break;
	  }

	case Bconcat3:
	  {
	    COMPILE_CALLN (Fconcat, 3);
	    break;
	  }

	case Bconcat4:
	  {
	    COMPILE_CALLN (Fconcat, 4);
	    break;
	  }

	case BconcatN:
	  {
	    op = FETCH;
	    COMPILE_CALLN (Fconcat, op);
	    break;
	  }

	case Bsub1:
	  unary_intmath (func, TOP, SUB1, &states[pc].label);
	  break;

	case Badd1:
	  unary_intmath (func, TOP, ADD1, &states[pc].label);
	  break;

	case Beqlsign:
	  {
	    jit_value_t v1 = POP;
	    compile_arithcompare (func, "=", TOP, v1, ARITH_EQUAL);
	    break;
	  }

	case Bgtr:
	  {
	    jit_value_t v1 = POP;
	    compile_arithcompare (func, ">", TOP, v1, ARITH_GRTR);
	    break;
	  }

	case Blss:
	  {
	    jit_value_t v1 = POP;
	    compile_arithcompare (func, "<", TOP, v1, ARITH_LESS);
	    break;
	  }

	case Bleq:
	  {
	    jit_value_t v1 = POP;
	    compile_arithcompare (func, "<=", TOP, v1, ARITH_LESS_OR_EQUAL);
	    break;
	  }

	case Bgeq:
	  {
	    jit_value_t v1 = POP;
	    compile_arithcompare (func, ">=", TOP, v1, ARITH_GRTR_OR_EQUAL);
	    break;
	  }

	case Bdiff:
	  {
	    COMPILE_CALLN (Fminus, 2);
	    break;
	  }

	case Bnegate:
	  unary_intmath (func, TOP, NEGATE, &states[pc].label);
	  break;

	case Bplus:
	  {
	    COMPILE_CALLN (Fplus, 2);
	    break;
	  }

	case Bmax:
	  {
	    COMPILE_CALLN (Fmax, 2);
	    break;
	  }

	case Bmin:
	  {
	    COMPILE_CALLN (Fmin, 2);
	    break;
	  }

	case Bmult:
	  {
	    COMPILE_CALLN (Ftimes, 2);
	    break;
	  }

	case Bquo:
	  {
	    COMPILE_CALLN (Fquo, 2);
	    break;
	  }

	case Brem:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "rem", Frem, TOP, v1);
	    break;
	  }

	case Bpoint:
	  PUSH (COMPILE_BUFFER_INT (pt));
	  break;

	case Bgoto_char:
	  compile_unary (func, "goto-char", Fgoto_char, TOP);
	  break;

	case Binsert:
	  {
	    COMPILE_CALLN (Finsert, 1);
	    break;
	  }

	case BinsertN:
	  {
	    op = FETCH;
	    COMPILE_CALLN (Finsert, op);
	    break;
	  }

	case Bpoint_max:
	  PUSH (COMPILE_BUFFER_INT (zv));
	  break;

	case Bpoint_min:
	  PUSH (COMPILE_BUFFER_INT (begv));
	  break;

	case Bchar_after:
	  compile_unary (func, "char-after", Fchar_after, TOP);
	  break;

	case Bfollowing_char:
	  PUSH (compile_nullary (func, "following-char", Ffollowing_char));
	  break;

	case Bpreceding_char:
	  PUSH (compile_nullary (func, "previous-char", Fprevious_char));
	  break;

	case Bcurrent_column:
	  PUSH (compile_nullary (func, "current-column", Fcurrent_column));
	  break;

	case Bindent_to:
	  {
	    jit_value_t tem = CONSTANT (func, Qnil);
	    compile_binary (func, "indent-to", Findent_to, TOP, tem);
	    break;
	  }

	case Beolp:
	  PUSH (compile_nullary (func, "eolp", Feolp));
	  break;

	case Beobp:
	  {
	    jit_value_t compare = COMPARE_BUFFER_INTS (pt, zv);
	    ++stack_pointer;
	    emit_qnil_or_qt (func, compare, TOP, &states[pc].label);
	    break;
	  }

	case Bbolp:
	  PUSH (compile_nullary (func, "bolp", Fbolp));
	  break;

	case Bbobp:
	  {
	    jit_value_t compare = COMPARE_BUFFER_INTS (pt, begv);
	    ++stack_pointer;
	    emit_qnil_or_qt (func, compare, TOP, &states[pc].label);
	    break;
	  }

	case Bcurrent_buffer:
	  PUSH (compile_nullary (func, "current-buffer", Fcurrent_buffer));
	  break;

	case Bset_buffer:
	  compile_unary (func, "set-buffer", Fset_buffer, TOP);
	  break;

	case Binteractive_p:	/* Obsolete since 24.1.  */
	  {
	    jit_value_t arg = CONSTANT (func, Qinteractive_p);
	    jit_value_t result = jit_insn_call_native (func, "interactive-p",
						       (void *) call0,
						       unary_signature, &arg, 1,
						       JIT_CALL_NOTHROW);
	    PUSH (result);
	    break;
	  }

	case Bforward_char:
	  compile_unary (func, "forward-char", Fforward_char, TOP);
	  break;

	case Bforward_word:
	  compile_unary (func, "forward-word", Fforward_word, TOP);
	  break;

	case Bskip_chars_forward:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "skip-chars-forward", Fskip_chars_forward,
			    TOP, v1);
	    break;
	  }

	case Bskip_chars_backward:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "skip-chars-backward", Fskip_chars_backward,
			    TOP, v1);
	    break;
	  }

	case Bforward_line:
	  compile_unary (func, "forward-line", Fforward_line, TOP);
	  break;

	case Bchar_syntax:
	  compile_unary (func, "char-syntax", Fchar_syntax, TOP);
	  break;

	case Bbuffer_substring:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "buffer-substring", Fbuffer_substring,
			    TOP, v1);
	    break;
	  }

	case Bdelete_region:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "delete-region", Fdelete_region, TOP, v1);
	    break;
	  }

	case Bnarrow_to_region:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "narrow-to-region", Fnarrow_to_region,
			    TOP, v1);
	    break;
	  }

	case Bwiden:
	  PUSH (compile_nullary (func, "widen", Fwiden));
	  break;

	case Bend_of_line:
	  compile_unary (func, "end-of-line", Fend_of_line, TOP);
	  break;

	case Bset_marker:
	  {
	    jit_value_t v2 = POP, v1 = POP;
	    compile_ternary (func, "set-marker", Fset_marker, TOP, v1, v2);
	    break;
	  }

	case Bmatch_beginning:
	  compile_unary (func, "match-beginning", Fmatch_beginning, TOP);
	  break;

	case Bmatch_end:
	  compile_unary (func, "match-end", Fmatch_end, TOP);
	  break;

	case Bupcase:
	  compile_unary (func, "upcase", Fupcase, TOP);
	  break;

	case Bdowncase:
	  compile_unary (func, "downcase", Fdowncase, TOP);
	  break;

	case Bstringeqlsign:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "string=", Fstring_equal, TOP, v1);
	    break;
	  }

	case Bstringlss:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "string<", Fstring_lessp, TOP, v1);
	    break;
	  }

	case Bequal:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "equal", Fequal, TOP, v1);
	    break;
	  }

	case Bnthcdr:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "nthcdr", Fnthcdr, TOP, v1);
	    break;
	  }

	case Belt:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "elt", Felt, TOP, v1);
	    break;
	  }

	case Bmember:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "member", Fmember, TOP, v1);
	    break;
	  }

	case Bassq:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "assq", Fassq, TOP, v1);
	    break;
	  }

	case Bnreverse:
	  compile_unary (func, "nreverse", Fnreverse, TOP);
	  break;

	case Bsetcar:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "setcar", Fsetcar, TOP, v1);
	    break;
	  }

	case Bsetcdr:
	  {
	    jit_value_t v1 = POP;
	    compile_binary (func, "setcdr", Fsetcdr, TOP, v1);
	    break;
	  }

	case Bcar_safe:
	  car_or_cdr (func, TOP, offsetof (struct Lisp_Cons, u.s.car),
		      &states[pc].label, true,
		      &called_wtype, &wtype_label, wtype_arg);
	  break;

	case Bcdr_safe:
	  car_or_cdr (func, TOP, offsetof (struct Lisp_Cons, u.s.u.cdr),
		      &states[pc].label, true,
		      &called_wtype, &wtype_label, wtype_arg);
	  break;

	case Bnconc:
	  {
	    COMPILE_CALLN (Fnconc, 2);
	    break;
	  }

	case Bnumberp:
	  {
	    jit_label_t push_t = jit_label_undefined;
	    jit_label_t push_nil = jit_label_undefined;

	    jit_value_t val = POP;
	    jit_value_t type;
	    jit_value_t compare = compare_integerp (func, val, &type);
	    jit_insn_branch_if (func, compare, &push_t);

	    jit_value_t type_val
	      = jit_value_create_nint_constant (func, jit_type_void_ptr,
						Lisp_Float);
	    compare = jit_insn_eq (func, type, type_val);

	    jit_insn_branch_if (func, compare, &push_t);

	    jit_insn_label (func, &push_nil);
	    PUSH (CONSTANT (func, Qnil));
	    jit_insn_branch (func, &states[pc].label);

	    --stack_pointer;
	    jit_insn_label (func, &push_t);
	    PUSH (CONSTANT (func, Qt));

	    break;
	  }

	case Bintegerp:
	  {
	    jit_value_t compare = compare_integerp (func, TOP, NULL);
	    emit_qnil_or_qt (func, compare, TOP, &states[pc].label);
	    break;
	  }

	  /* Handy byte-codes for lexical binding.  */
	case Bstack_ref1:
	case Bstack_ref2:
	case Bstack_ref3:
	case Bstack_ref4:
	case Bstack_ref5:
	  {
	    jit_value_t v1 = stack[stack_pointer - (op - Bstack_ref)];
	    PUSH (v1);
	    break;
	  }
	case Bstack_ref6:
	  {
	    jit_value_t v1 = stack[stack_pointer - FETCH];
	    PUSH (v1);
	    break;
	  }
	case Bstack_ref7:
	  {
	    jit_value_t v1 = stack[stack_pointer - FETCH2];
	    PUSH (v1);
	    break;
	  }
	case Bstack_set:
	  /* stack-set-0 = discard; stack-set-1 = discard-1-preserve-tos.  */
	  {
	    jit_value_t tos = POP;
	    op = FETCH;
	    if (op > 0)
	      jit_insn_store (func, stack[stack_pointer + 1 - op], tos);
	    break;
	  }
	case Bstack_set2:
	  {
	    jit_value_t tos = POP;
	    op = FETCH2;
	    if (op > 0)
	      jit_insn_store (func, stack[stack_pointer + 1 - op], tos);
	    break;
	  }
	case BdiscardN:
	  op = FETCH;
	  if (op & 0x80)
	    {
	      op &= 0x7F;
	      jit_insn_store (func, stack[stack_pointer - op], TOP);
	    }
	  DISCARD (op);
	  break;

        case Bswitch:
	  {
	    Lisp_Object htab
	      = vectorp[states[orig_pc].stack[stack_pointer]];
            struct Lisp_Hash_Table *h = XHASH_TABLE (htab);

	    /* Pop the hash table from the stack.  */
	    --stack_pointer;

	    /* Minimum and maximum PC values for the table.  */
	    EMACS_INT min_pc = bytestr_length, max_pc = 0;
	    if (!find_hash_min_max_pc (h, &min_pc, &max_pc))
	      goto fail;

	    jit_value_t args[3];
	    args[0] = POP;
	    args[1] = CONSTANT (func, htab);
	    args[2] = CONSTANT (func, Qnil);

	    jit_value_t value
	      = jit_insn_call_native (func, "Fgethash", (void *) Fgethash,
				      ternary_signature, args, 3,
				      JIT_CALL_NOTHROW);
	    jit_value_t compare = jit_insn_eq (func, value, args[2]);

	    jit_label_t default_case = jit_function_reserve_label (func);
	    jit_insn_branch_if (func, compare, &default_case);

	    /* Note that we know the type because we check it when
	       walking the hash table, and the hash table is
	       (effectively) immutable.  */
	    value = to_int (func, value);
	    jit_value_t min_pc_val
	      = jit_value_create_nint_constant (func, jit_type_sys_int,
						min_pc);
	    value = jit_insn_sub (func, value, min_pc_val);

	    /* Initialize switch labels.  */
	    for (int i = 0; i < max_pc - min_pc; ++i)
	      sw_labels[i] = default_case;

	    /* Fill in the switch table.  */
	    for (ptrdiff_t i = 0; i < HASH_TABLE_SIZE (h); ++i)
	      {
		if (!NILP (HASH_HASH (h, i)))
		  {
		    Lisp_Object pc = HASH_VALUE (h, i);
		    /* This was already checked by
		       find_hash_min_max_pc.  */
		    eassert (INTEGERP (pc));
		    EMACS_INT pcval = XINT (pc);

		    /* Make sure that the label we'll need is defined.  */
		    if (states[pcval].label == jit_label_undefined)
		      states[pcval].label = jit_function_reserve_label (func);

		    sw_labels[pcval - min_pc] = states[pcval].label;
		  }
	      }

	    jit_insn_jump_table (func, value, sw_labels, max_pc - min_pc);
	    jit_insn_label (func, &default_case);
	    break;
	  }

	case Bconstant2:
	  {
	    op = FETCH2;
	    jit_value_t c = CONSTANT (func, vectorp[op]);
	    PUSH (c);
	  }
	  break;

	case Bconstant:
	default:
	  {
	    if (op >= Bconstant && op < Bconstant + vector_size)
	      {
		jit_value_t c = CONSTANT (func, vectorp[op - Bconstant]);
		PUSH (c);
	      }
	    else
	      goto fail;
	  }
	  break;
	}

      /* Double check that the prepass and this pass agree about stack
	 effects.  */
      eassert (orig_pc == -1 || states[pc].stack_depth == stack_pointer + 1);
    }

  if (scratch_slots_needed > 0)
    {
      jit_label_t init_start = jit_label_undefined;
      jit_label_t init_end = jit_label_undefined;

      jit_insn_label (func, &init_start);
      jit_value_t scratch_size
	= jit_value_create_nint_constant (func, jit_type_sys_int,
					  (scratch_slots_needed
					   * sizeof (Lisp_Object)));
      jit_value_t allocated = jit_insn_alloca (func, scratch_size);
      jit_insn_store (func, scratch, allocated);
      jit_insn_label (func, &init_end);

      jit_insn_move_blocks_to_start (func, init_start, init_end);
    }

  if (need_argfail)
    {
      jit_value_t args[3];
      jit_insn_label (func, &argfail);
      args[0] = mandatory_val;
      args[1] = nonrest_val;
      args[2] = n_args;
      jit_insn_call_native (func, "wrong-number-of-arguments",
			    (void *) wrong_number_of_arguments,
			    wrong_number_of_arguments_signature,
			    args, 3, JIT_CALL_NORETURN);
    }

  if (called_wtype)
    compile_wrong_type_argument (func, &wtype_label, wtype_arg);

  if (!jit_function_compile (func))
    {
      /* Boo!  */
    fail:
      jit_function_abandon (func);
      result = NULL;
    }
  else
    {
      /* FIXME bogus cast.  */
      result->function.a0
	= (Lisp_Object (*) (void)) jit_function_to_closure (func);
    }

  xfree (stack);
  xfree (sw_labels);
  free_state (states, bytestr_length);

  return result;
}

static bool
emacs_jit_compile_nolock (Lisp_Object func)
{
  if (!COMPILEDP (func))
    return false;

  Lisp_Object bytestr = AREF (func, COMPILED_BYTECODE);
  /* FIXME handling the STRING_MULTIBYTE case - is it important?  */
  if (!STRINGP (bytestr) || STRING_MULTIBYTE (bytestr))
    return false;

  ptrdiff_t bytestr_length = SBYTES (bytestr);

  Lisp_Object vector = AREF (func, COMPILED_CONSTANTS);
  if (!VECTORP (vector))
    return false;

  Lisp_Object *vectorp = XVECTOR (vector)->contents;

  Lisp_Object maxdepth = AREF (func, COMPILED_STACK_DEPTH);
  if (!NATNUMP (maxdepth))
    return false;

  struct subr_function *subr = compile (bytestr_length, SDATA (bytestr),
					XFASTINT (maxdepth) + 1,
					vectorp, ASIZE (vector),
					AREF (func, COMPILED_ARGLIST));

  XVECTOR (func)->contents[COMPILED_JIT_CODE] = (Lisp_Object) subr;

  return subr != NULL;
}

void
emacs_jit_compile (Lisp_Object func)
{
  if (!emacs_jit_initialized)
    return;

  Lisp_Object bytestr = AREF (func, COMPILED_BYTECODE);
  CHECK_STRING (bytestr);

  Lisp_Object vector = AREF (func, COMPILED_CONSTANTS);
  CHECK_VECTOR (vector);

  Lisp_Object maxdepth = AREF (func, COMPILED_STACK_DEPTH);
  CHECK_NATNUM (maxdepth);

  jit_context_build_start (emacs_jit_context);
  emacs_jit_compile_nolock (func);
  jit_context_build_end (emacs_jit_context);
}

DEFUN ("jit-compile", Fjit_compile, Sjit_compile,
       1, 1, 0,
       doc: /* JIT compile a function.  */)
  (Lisp_Object func)
{
  struct Lisp_Vector *vec;

  if (!COMPILEDP (func))
    error ("Not a byte-compiled function");

  vec = XVECTOR (func);
  if (vec->contents[COMPILED_JIT_CODE] == NULL)
    emacs_jit_compile (func);

  return Qnil;
}

DEFUN ("jit-disassemble-to-string", Fjit_disassemble_to_string,
       Sjit_disassemble_to_string, 1, 1, 0,
       doc: /* Disassemble a JIT-compiled function and return a string with the disassembly.  */)
  (Lisp_Object func)
{
  char *buffer = NULL;
  size_t size = 0;
  FILE *stream;
  Lisp_Object str;
  struct Lisp_Vector *vec;
  jit_function_t cfunc;
  struct subr_function *sfunc;

  if (!COMPILEDP (func))
    error ("Not a byte-compiled function");

#ifdef HAVE_OPEN_MEMSTREAM
  vec = XVECTOR (func);
  sfunc = (struct subr_function *) vec->contents[COMPILED_JIT_CODE];
  if (sfunc == NULL)
    error ("Not JIT-compiled");

  cfunc = jit_function_from_closure (emacs_jit_context, sfunc->function.a0);
  stream = open_memstream (&buffer, &size);
  jit_dump_function (stream, cfunc, "Function");
  fclose (stream);

  str = make_string (buffer, size);

  xfree (buffer);
  return str;
#else
  error ("Cannot disassemble JIT code in this build: open_memstream missing");
#endif
}

/* It might happen that we can't recompile a symbol's function -- for
   example, it could be redefined as a subr or a sexp.  In this case,
   any lazily-compiled function will just be a trampoline that calls
   via Ffuncall.  */
static void
compile_ffuncall_trampoline (jit_function_t func, int arity, Lisp_Object sym)
{
  jit_value_t scratch_size
    = jit_value_create_nint_constant (func, jit_type_sys_int,
				      ((arity + 1) * sizeof (Lisp_Object)));
  jit_value_t allocated = jit_insn_alloca (func, scratch_size);

  jit_value_t incoming[SUBR_MAX_ARGS + 1];
  incoming[0] = CONSTANT (func, sym);
  for (int i = 0; i < arity; ++i)
    incoming[i + 1] = jit_value_get_param (func, i);

  jit_value_t result = compile_callN (func, "Ffuncall", Ffuncall, arity + 1,
				      allocated, incoming);
  jit_insn_return (func, result);
}

/* Compile a trampoline that takes ACTUAL arguments and then calls the
   function having NONREST arguments, where ACTUAL < NONREST and
   NONREST < SUBR_MAX_ARGS.  */
static void
compile_forwarding_call (jit_function_t func, int actual, int nonrest)
{
  jit_value_t incoming[SUBR_MAX_ARGS];

  for (int i = 0; i < actual; ++i)
    incoming[i] = jit_value_get_param (func, i);

  jit_value_t qnil = CONSTANT (func, Qnil);
  for (int i = actual; i < nonrest; ++i)
    incoming[i] = qnil;

  jit_value_t result = jit_insn_call (func, NULL, fixme,
				      fixme_signature, incoming, nonrest,
				      JIT_CALL_NORETURN);
  jit_insn_return (func, result);
}

static blah
lazily_compile_function (jit_function_t func)
{
  struct Lisp_Symbol *sym = jit_function_get_meta (func, 0);
  int arity = (int) jit_function_get_meta (func, 1);


}

static void
require_one_function (struct Lisp_Symbol *sym, int n_args)
{
  if (n_args >= SUBR_MAX_ARGS)
    n_args = SUBR_MAX_ARGS + 1;

  if (sym->functions == NULL)
    {
      /* FIXME something has to free this.  */
      sym->functions = xzalloc (sizeof (struct emacs_jit_functions));
    }

  if (sym->functions->entries[n_args].function != NULL)
    return;

  jit_function_t func  = jit_function_create (emacs_jit_context,
					      subr_signature[n_args]);
  sym->functions->entries[i].function = func;
  jit_function_set_meta (func, 0, sym, NULL, 0);
  jit_function_set_meta (func, 1, (void *) n_args, NULL, 0);
  jit_function_set_on_demand_compiler (func, lazily_compile_function);

  /* FIXME bogus cast.  */
  sym->functions->entries[i].entry = jit_function_to_closure (func);
}

void
recompile_functions (struct Lisp_Symbol *sym)
{
  if (sym->functions == NULL)
    return;

  // if NILP -> compile a single thing to throw an exception
  // otherwise figure out which one is canonical
  // actually no these are all lazy
  for (int i = 0; i < N_FUNCTION_SLOTS; ++i)
    {
      if (sym->functions->entries[i].function.a0 == NULL)
	continue;

      /* FIXME nothing is freeing the old functions - we must attach
	 them to the bytecode object so they can be GC'd
	 appropriately.  */
      sym->functions->entries[i].function.a0 = NULL;

      require_one_function (sym, i);
    }
}

void
syms_of_jit (void)
{
  defsubr (&Sjit_compile);
  defsubr (&Sjit_disassemble_to_string);
  DEFSYM (Qinteractive_p, "interactive-p");

  DEFVAR_LISP ("jit-direct-call-subrs", Vjit_direct_call_subrs,
	       doc: /* A list of functions that can be called directly.
Ordinarily the JIT will emit an indirect call via a symbol, but
if a subr is listed here.  Direct calls are not affected
by advice, so this should be used sparingly.  */);
}

void
init_jit (void)
{
#define LEN SUBR_MAX_ARGS + 1

  jit_type_t params[LEN];
  int i;

  jit_init ();
  emacs_jit_context = jit_context_create ();

  if (sizeof (ptrdiff_t) == 8)
    ptrdiff_t_type = jit_type_ulong;
  else
    {
      eassert (sizeof (ptrdiff_t) == 4);
      ptrdiff_t_type = jit_type_uint;
    }

  for (i = 0; i < LEN; ++i)
    params[i] = jit_type_void_ptr;

  for (i = 0; i < SUBR_MAX_ARGS; ++i)
    subr_signature[i] = jit_type_create_signature (jit_abi_cdecl,
						   jit_type_void_ptr,
						   params, i, 1);

  nullary_signature = jit_type_create_signature (jit_abi_cdecl,
						 jit_type_void_ptr, params, 0,
						 1);
  unary_signature = jit_type_create_signature (jit_abi_cdecl,
					       jit_type_void_ptr, params, 1,
					       1);
  binary_signature = jit_type_create_signature (jit_abi_cdecl,
						jit_type_void_ptr, params, 2,
						1);
  ternary_signature = jit_type_create_signature (jit_abi_cdecl,
						 jit_type_void_ptr, params, 3,
						 1);
  specbind_signature = jit_type_create_signature (jit_abi_cdecl,
						  jit_type_void, params, 2, 1);
  save_excursion_save_signature = jit_type_create_signature (jit_abi_cdecl,
							     jit_type_void_ptr,
							     NULL, 0, 1);
  record_unwind_protect_signature
    = jit_type_create_signature (jit_abi_cdecl, jit_type_void, params, 2, 1);
  void_void_signature = jit_type_create_signature (jit_abi_cdecl,
						   jit_type_void, NULL, 0, 1);

  temp_output_buffer_show_signature
    = jit_type_create_signature (jit_abi_cdecl, jit_type_void_ptr,
				 params, 1, 1);

  params[2] = jit_type_sys_int;
  arithcompare_signature = jit_type_create_signature (jit_abi_cdecl,
						      jit_type_void_ptr,
						      params, 3, 1);

  params[0] = jit_type_sys_int;
  unbind_n_signature = jit_type_create_signature (jit_abi_cdecl,
						  jit_type_void_ptr, params, 1,
						  1);

  params[0] = ptrdiff_t_type;
  callN_signature = jit_type_create_signature (jit_abi_cdecl,
					       jit_type_void_ptr, params, 2,
					       1);
  compiled_signature = callN_signature;
  subr_signature[SUBR_MAX_ARGS] = callN_signature;

  params[0] = jit_type_sys_int;
  params[1] = jit_type_sys_int;
  params[2] = jit_type_sys_int;
  wrong_number_of_arguments_signature
    = jit_type_create_signature (jit_abi_cdecl, jit_type_void, params, 3, 1);

  params[0] = jit_type_void_ptr;
  params[1] = jit_type_void_ptr;
  params[2] = jit_type_void_ptr;
  params[3] = jit_type_sys_int;
  set_internal_signature
    = jit_type_create_signature (jit_abi_cdecl, jit_type_void, params, 4, 1);

  setjmp_signature = jit_type_create_signature (jit_abi_cdecl,
						jit_type_sys_int,
						params, 1, 1);

  params[1] = jit_type_sys_int;
  push_handler_signature = jit_type_create_signature (jit_abi_cdecl,
						      jit_type_void_ptr,
						      params, 2, 1);

  emacs_jit_initialized = true;
}

#endif /* HAVE_LIBJIT */
