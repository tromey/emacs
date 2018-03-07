/* Byte code definitions
   Copyright (C) 1985-1988, 1993, 2000-2018 Free Software Foundation,
   Inc.

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

#ifndef EMACS_BYTECODE_H
#define EMACS_BYTECODE_H

/* To use this macro, define the "DEFINE" macro to expand to what
   you'd like.  Each DEFINE call is of the form:
   DEFINE (NAME, NUMBER, TO-POP, TO-PUSH)
   NAME is the bytecode's name.
   NUMBER is the bytecode's value.
   TO-POP is how many values are popped from the expression stack by
   this opcode.
   TO-PUSH is how many values are pushed on the expression stack by
   this opcode.
   Note that the push/pop values are not truly complete: a few
   bytecodes have to be handled specially.  In these cases, TO-POP
   will be -1.
   Also note that many Bconstant forms are not handled here.
*/

#define BYTE_CODES							\
DEFINE (Bstack_ref, 0, -2, -2) /* Actually, Bstack_ref+0 is not implemented: use dup.  */ \
DEFINE (Bstack_ref1, 1, -1, -1)						\
DEFINE (Bstack_ref2, 2, -1, -1)						\
DEFINE (Bstack_ref3, 3, -1, -1)						\
DEFINE (Bstack_ref4, 4, -1, -1)						\
DEFINE (Bstack_ref5, 5, -1, -1)						\
DEFINE (Bstack_ref6, 6, -1, -1)						\
DEFINE (Bstack_ref7, 7, -1, -1)						\
DEFINE (Bvarref, 010, 0, 1)						\
DEFINE (Bvarref1, 011, 0, 1)						\
DEFINE (Bvarref2, 012, 0, 1)						\
DEFINE (Bvarref3, 013, 0, 1)						\
DEFINE (Bvarref4, 014, 0, 1)						\
DEFINE (Bvarref5, 015, 0, 1)						\
DEFINE (Bvarref6, 016, 0, 1)						\
DEFINE (Bvarref7, 017, 0, 1)						\
DEFINE (Bvarset, 020, 1, 0)						\
DEFINE (Bvarset1, 021, 1, 0)						\
DEFINE (Bvarset2, 022, 1, 0)						\
DEFINE (Bvarset3, 023, 1, 0)						\
DEFINE (Bvarset4, 024, 1, 0)						\
DEFINE (Bvarset5, 025, 1, 0)						\
DEFINE (Bvarset6, 026, 1, 0)						\
DEFINE (Bvarset7, 027, 1, 0)						\
DEFINE (Bvarbind, 030, 1, 0)						\
DEFINE (Bvarbind1, 031, 1, 0)						\
DEFINE (Bvarbind2, 032, 1, 0)						\
DEFINE (Bvarbind3, 033, 1, 0)						\
DEFINE (Bvarbind4, 034, 1, 0)						\
DEFINE (Bvarbind5, 035, 1, 0)						\
DEFINE (Bvarbind6, 036, 1, 0)						\
DEFINE (Bvarbind7, 037, 1, 0)						\
DEFINE (Bcall, 040, 1, 1)						\
DEFINE (Bcall1, 041, 2, 1)						\
DEFINE (Bcall2, 042, 3, 1)						\
DEFINE (Bcall3, 043, 4, 1)						\
DEFINE (Bcall4, 044, 5, 1)						\
DEFINE (Bcall5, 045, 6, 1)						\
DEFINE (Bcall6, 046, -1, -1)						\
DEFINE (Bcall7, 047, -1, -1)						\
DEFINE (Bunbind, 050, 0, 0)						\
DEFINE (Bunbind1, 051, 0, 0)						\
DEFINE (Bunbind2, 052, 0, 0)						\
DEFINE (Bunbind3, 053, 0, 0)						\
DEFINE (Bunbind4, 054, 0, 0)						\
DEFINE (Bunbind5, 055, 0, 0)						\
DEFINE (Bunbind6, 056, 0, 0)						\
DEFINE (Bunbind7, 057, 0, 0)						\
									\
DEFINE (Bpophandler, 060, 0, 0)						\
DEFINE (Bpushconditioncase, 061, -1, -1)				\
DEFINE (Bpushcatch, 062, -11, -1)					\
									\
DEFINE (Bnth, 070, 2, 1)						\
DEFINE (Bsymbolp, 071, 1, 1)						\
DEFINE (Bconsp, 072, 1, 1)						\
DEFINE (Bstringp, 073, 1, 1)						\
DEFINE (Blistp, 074, 1, 1)						\
DEFINE (Beq, 075, 2, 1)							\
DEFINE (Bmemq, 076, 2, 1)						\
DEFINE (Bnot, 077, 1, 1)						\
DEFINE (Bcar, 0100, 1, 1)						\
DEFINE (Bcdr, 0101, 1, 1)						\
DEFINE (Bcons, 0102, 2, 1)						\
DEFINE (Blist1, 0103, 1, 1)						\
DEFINE (Blist2, 0104, 2, 1)						\
DEFINE (Blist3, 0105, 3, 1)						\
DEFINE (Blist4, 0106, 4, 1)						\
DEFINE (Blength, 0107, 1, 1)						\
DEFINE (Baref, 0110, 2, 1)						\
DEFINE (Baset, 0111, 2, 1)						\
DEFINE (Bsymbol_value, 0112, 1, 1)					\
DEFINE (Bsymbol_function, 0113, 1, 1)					\
DEFINE (Bset, 0114, 2, 1)						\
DEFINE (Bfset, 0115, 2, 1)						\
DEFINE (Bget, 0116, 2, 1)						\
DEFINE (Bsubstring, 0117, 3, 1)						\
DEFINE (Bconcat2, 0120, 2, 1)						\
DEFINE (Bconcat3, 0121, 3, 1)						\
DEFINE (Bconcat4, 0122, 4, 1)						\
DEFINE (Bsub1, 0123, 1, 1)						\
DEFINE (Badd1, 0124, 1, 1)						\
DEFINE (Beqlsign, 0125, 2, 1)						\
DEFINE (Bgtr, 0126, 2, 1)						\
DEFINE (Blss, 0127, 2, 1)						\
DEFINE (Bleq, 0130, 2, 1)						\
DEFINE (Bgeq, 0131, 2, 1)						\
DEFINE (Bdiff, 0132, 2, 1)						\
DEFINE (Bnegate, 0133, 1, 1)						\
DEFINE (Bplus, 0134, 2, 1)						\
DEFINE (Bmax, 0135, 2, 1)						\
DEFINE (Bmin, 0136, 2, 1)						\
DEFINE (Bmult, 0137, 2, 1)						\
									\
DEFINE (Bpoint, 0140, 0, 1)						\
/* Was Bmark in v17.  */						\
DEFINE (Bsave_current_buffer, 0141, 0, 0) /* Obsolete.  */		\
DEFINE (Bgoto_char, 0142, 1, 1)						\
DEFINE (Binsert, 0143, 1, 1)						\
DEFINE (Bpoint_max, 0144, 0, 1)						\
DEFINE (Bpoint_min, 0145, 0, 1)						\
DEFINE (Bchar_after, 0146, 1, 1)					\
DEFINE (Bfollowing_char, 0147, 0, 1)					\
DEFINE (Bpreceding_char, 0150, 0, 1)					\
DEFINE (Bcurrent_column, 0151, 0, 1)					\
DEFINE (Bindent_to, 0152, 1, 1)						\
DEFINE (Beolp, 0154, 0, 1)						\
DEFINE (Beobp, 0155, 0, 1)						\
DEFINE (Bbolp, 0156, 0, 1)						\
DEFINE (Bbobp, 0157, 0, 1)						\
DEFINE (Bcurrent_buffer, 0160, 0, 1)					\
DEFINE (Bset_buffer, 0161, 1, 1)					\
DEFINE (Bsave_current_buffer_1, 0162, 0, 0) /* Replacing Bsave_current_buffer.  */ \
DEFINE (Binteractive_p, 0164, 0, 1) /* Obsolete since Emacs-24.1.  */	\
									\
DEFINE (Bforward_char, 0165, 1, 1)					\
DEFINE (Bforward_word, 0166, 1, 1)					\
DEFINE (Bskip_chars_forward, 0167, 2, 1)				\
DEFINE (Bskip_chars_backward, 0170, 2, 1)				\
DEFINE (Bforward_line, 0171, 1, 1)					\
DEFINE (Bchar_syntax, 0172, 1, 1)					\
DEFINE (Bbuffer_substring, 0173, 2, 1)					\
DEFINE (Bdelete_region, 0174, 2, 1)					\
DEFINE (Bnarrow_to_region, 0175, 2, 1)					\
DEFINE (Bwiden, 0176, 0, 1)						\
DEFINE (Bend_of_line, 0177, 1, 1)					\
									\
DEFINE (Bconstant2, 0201, -1, -1)						\
DEFINE (Bgoto, 0202, 0, 0)						\
DEFINE (Bgotoifnil, 0203, 1, 0)						\
DEFINE (Bgotoifnonnil, 0204, 1, 0)					\
DEFINE (Bgotoifnilelsepop, 0205, -1, -1)				\
DEFINE (Bgotoifnonnilelsepop, 0206, -1, -1)				\
DEFINE (Breturn, 0207, 1, 0)						\
DEFINE (Bdiscard, 0210, 1, 0)						\
DEFINE (Bdup, 0211, -1, -1)						\
									\
DEFINE (Bsave_excursion, 0212, 0, 0)					\
DEFINE (Bsave_window_excursion, 0213, 1, 1) /* Obsolete since Emacs-24.1.  */ \
DEFINE (Bsave_restriction, 0214, 0, 0)					\
DEFINE (Bcatch, 0215, 2, 1)						\
									\
DEFINE (Bunwind_protect, 0216, 1, 0)					\
DEFINE (Bcondition_case, 0217, 3, 1)					\
DEFINE (Btemp_output_buffer_setup, 0220, 1, 1) /* Obsolete since Emacs-24.1.  */ \
DEFINE (Btemp_output_buffer_show, 0221, 2, 1)  /* Obsolete since Emacs-24.1.  */ \
									\
DEFINE (Bunbind_all, 0222, 0, 0)	/* Obsolete.  Never used.  */	\
									\
DEFINE (Bset_marker, 0223, 3, 1)					\
DEFINE (Bmatch_beginning, 0224, 1, 1)					\
DEFINE (Bmatch_end, 0225, 1, 1)						\
DEFINE (Bupcase, 0226, 1, 1)						\
DEFINE (Bdowncase, 0227, 1, 1)						\
									\
DEFINE (Bstringeqlsign, 0230, 2, 1)					\
DEFINE (Bstringlss, 0231, 2, 1)						\
DEFINE (Bequal, 0232, 2, 1)						\
DEFINE (Bnthcdr, 0233, 2, 1)						\
DEFINE (Belt, 0234, 2, 1)						\
DEFINE (Bmember, 0235, 2, 1)						\
DEFINE (Bassq, 0236, 2, 1)						\
DEFINE (Bnreverse, 0237, 1, 1)						\
DEFINE (Bsetcar, 0240, 2, 1)						\
DEFINE (Bsetcdr, 0241, 2, 1)						\
DEFINE (Bcar_safe, 0242, 1, 1)						\
DEFINE (Bcdr_safe, 0243, 1, 1)						\
DEFINE (Bnconc, 0244, 2, 1)						\
DEFINE (Bquo, 0245, 2, 1)						\
DEFINE (Brem, 0246, 2, 1)						\
DEFINE (Bnumberp, 0247, 1, 1)						\
DEFINE (Bintegerp, 0250, 1, 1)						\
									\
DEFINE (BRgoto, 0252, 0, 0)						\
DEFINE (BRgotoifnil, 0253, 1, 0)					\
DEFINE (BRgotoifnonnil, 0254, 1, 0)					\
DEFINE (BRgotoifnilelsepop, 0255, -1, -1)				\
DEFINE (BRgotoifnonnilelsepop, 0256, -1, -1)				\
									\
DEFINE (BlistN, 0257, -1, -1)						\
DEFINE (BconcatN, 0260, -1, -1)						\
DEFINE (BinsertN, 0261, -1, -1)						\
									\
/* Bstack_ref is code 0.  */						\
DEFINE (Bstack_set,  0262, -1, -1)					\
DEFINE (Bstack_set2, 0263, -1, -1)					\
DEFINE (BdiscardN,   0266, -1, -1)					\
									\
DEFINE (Bswitch, 0267, -1, -1)						\
                                                                        \
DEFINE (Bconstant, 0300, -1, -1)

enum byte_code_op
{
#define DEFINE(name, value, _ignore1, _ignore2) name = value,
    BYTE_CODES
#undef DEFINE

#if BYTE_CODE_SAFE
    Bscan_buffer = 0153, /* No longer generated as of v18.  */
    Bset_mark = 0163, /* this loser is no longer generated as of v18 */
#endif
};

#endif /* EMACS_BYTECODE_H */
