/*********************************************************************************/
/*                OCaml-R                                                        */
/*                                                                               */
/*    Copyright (C) 2008-2010 Institut National de Recherche en                  */
/*    Informatique et en Automatique. All rights reserved.                       */
/*                                                                               */
/*    Copyright (C) 2009-2010 Guillaume Yziquel. All rights reserved.            */
/*                                                                               */
/*    This program is free software; you can redistribute it and/or modify       */
/*    it under the terms of the GNU General Public License as                    */
/*    published by the Free Software Foundation; either version 3 of the         */
/*    License, or  any later version.                                            */
/*                                                                               */
/*    This program is distributed in the hope that it will be useful,            */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               */
/*    GNU Library General Public License for more details.                       */
/*                                                                               */
/*    You should have received a copy of the GNU General Public                  */
/*    License along with this program; if not, write to the Free Software        */
/*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   */
/*    02111-1307  USA                                                            */
/*                                                                               */
/*    Contact: Maxence.Guesdon@inria.fr                                          */
/*             guillaume.yziquel@citycable.ch                                    */
/*********************************************************************************/

#define USE_RINTERNALS /* This compilation directive allows us to have access to
                          the definition of R internal types. Compilation of the
                          inspect* functions is otherwise prohibited. */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/signals.h>
#include <caml/custom.h>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rinterface.h>
#include <Rembedded.h>
#include <R_ext/Parse.h>
#include <stdio.h>

#include "databridge.h"

/* Low-level data manipulation functions. */

/* What follows is low-level accessor functions, in order to inspect
   in details the contents of SEXPs and VECSEXPs. The implementation
   details are in Rinternals.h, enclosed in #ifdef USE_RINTERNALS
   directives, making these opaque pointers when not turned on. */

/* Concerning VECSEXPs: */

/* Quotation from section 1.1.3 "The 'data'" of "R Internals" (R-ints.pdf):
   'This [i.e. vecsxp.truelength] is almost unused. The only current use is for
   hash tables of environments (VECSXPs), where length is the size of the table
   and truelength is the number of primary slots in use, and for the reference
   hash tables in serialization (VECSXPs),where truelength is the number of
   slots in use.' */

/* CHARSXPs are in fact VECSEXPs. Concerning encoding, quotation from section 1.1.2
   "Rest_of_header" of "R internals" (R-ints.pdf): 'As from R 2.5.0, bits 2 and 3 for
   a CHARSXP are used to note that it is known to be in Latin-1 and UTF-8 respectively.
   (These are not usually set if it is also known to be in ASCII, since code does not
   need to know the charset to handle ASCII strings. From R 2.8.0 it is guaranteed that
   they will not be set for CHARSXPs created by R itself.) As from R 2.8.0 bit 5 is used
   to indicate that a CHARSXP is hashed by its address, that is NA STRING or in the
   CHARSXP cache. */


/**  Returns the attributes of an R value.
  *
  *  @param sexp An R value.
  *  @return The attributes of this value, as a pairlist.
  */
CAMLprim value ocamlr_inspect_attributes (value sexp) {
  return(Val_sexp(ATTRIB(Sexp_val(sexp))));
}


/**  Returns the length of a VECSEXP.
  *
  *  @param vecsexp An R vector value.
  *  @return Its length, i.e. the number of elements.
  */
CAMLprim value ocamlr_inspect_vecsxp_length (value vecsexp) {
  return(Val_int(LENGTH(Sexp_val(vecsexp))));
}



/* Concerning various types of SEXPs: */


/**  Returns the offset of an R primitive function.
  *
  *  @note: This function bypasses the provided API, and requires the
  *         use of the #define USE_RINTERNALS directive.
  *
  *  @param sexp An R value which is a primitive function.
  *  @return Its offset in the R table of primitives.
  */
CAMLprim value ocamlr_inspect_primsxp_offset (value sexp) {
  return(Val_int(Sexp_val(sexp)->u.primsxp.offset));
}


/**  Returns the name of an R symbol.
  *
  *  @param sexp An R value of sexptype SYMSXP.
  *  @return The name of the R symbol.
  */
CAMLprim value ocamlr_inspect_symsxp_pname (value sexp) {
  return(Val_sexp(PRINTNAME(Sexp_val(sexp))));
}


/**  Returns the value of a symbol.
  *
  *  @param sexp An R value of sexptype SYMSXP.
  *  @return The value of the R symbol.
  */
CAMLprim value ocamlr_inspect_symsxp_value (value sexp) {
  return(Val_sexp(SYMVALUE(Sexp_val(sexp))));
}


/**  Returns the internal sexp of a symbol.
  *
  *  @param sexp An R value of sexptype SYMSXP.
  *  @return The internal value of a symbol.
  */
CAMLprim value ocamlr_inspect_symsxp_internal (value sexp) {
  return(Val_sexp(INTERNAL(Sexp_val(sexp))));
}


/**  Returns the head element of a pairlist.
  *
  *  @param sexp An R pairlist.
  *  @return The head element of the R pairlist.
  */
CAMLprim value ocamlr_inspect_listsxp_carval (value sexp) {
  return(Val_sexp(CAR(Sexp_val(sexp))));
}


/**  Returns the tail pairlist of a pairlist.
  *
  *  @param sexp An R pairlist.
  *  @return The tail pairlist of the R pairlist.
  */
CAMLprim value ocamlr_inspect_listsxp_cdrval (value sexp) {
  return(Val_sexp(CDR(Sexp_val(sexp))));
}


/**  Returns the tag value of the head element of a pairlist.
  *
  *  @param sexp An R pairlist.
  *  @return The tag of the head of the R pairlist.
  */
CAMLprim value ocamlr_inspect_listsxp_tagval (value sexp) {
  return(Val_sexp(TAG(Sexp_val(sexp))));
}


/**  Returns the frame of an environment.
  *
  *  @param sexp An R environment.
  *  @return The frame of the R environment.
  */
CAMLprim value ocamlr_inspect_envsxp_frame (value sexp) {
  return(Val_sexp(FRAME(Sexp_val(sexp))));
}


/**  Returns the enclosing environment of an environment.
  *
  *  @param sexp An R environment.
  *  @return The enclosing environmnent of the R environment.
  */
CAMLprim value ocamlr_inspect_envsxp_enclos (value sexp) {
  return(Val_sexp(ENCLOS(Sexp_val(sexp))));
}


/**  Returns the hash table of an environment.
  *
  *  @param sexp An R environment.
  *  @return The hash table of the R environment.
  */
CAMLprim value ocamlr_inspect_envsxp_hashtab (value sexp) {
  return(Val_sexp(HASHTAB(Sexp_val(sexp))));
}


/**  Returns the list of formal arguments of a closure.
  *
  *  @param sexp An R closure.
  *  @return The list of formal arguments of the R closure.
  */
CAMLprim value ocamlr_inspect_closxp_formals (value sexp) {
  return(Val_sexp(FORMALS(Sexp_val(sexp))));
}


/**  Returns the body of a closure.
  *
  *  @param sexp An R closure.
  *  @return The body of the R closure.
  */
CAMLprim value ocamlr_inspect_closxp_body (value sexp) {
  return(Val_sexp(BODY(Sexp_val(sexp))));
}


/**  Returns the environment of a closure.
  *
  *  @param sexp An R closure.
  *  @return The environment of the R closure.
  */
CAMLprim value ocamlr_inspect_closxp_env (value sexp) {
  return(Val_sexp(CLOENV(Sexp_val(sexp))));
}


/**  Returns the value of a promise.
  *
  *  @note This function bypasses the provided API, and requires the
  *        use of the #define USE_RINTERNALS directive.
  *
  *  @param sexp An R promise.
  *  @return The value of the R promise.
  */
CAMLprim value ocamlr_inspect_promsxp_value (value sexp) {
  return(Val_sexp(Sexp_val(sexp)->u.promsxp.value));
}


/**  Returns the expression of a promise.
  *
  *  @note This function bypasses the provided R API, and requires the
  *        use of the #define USE_RINTERNALS directive.
  *
  *  @param sexp An R promise.
  *  @return The expression of the R promise.
  */
CAMLprim value ocamlr_inspect_promsxp_expr (value sexp) {
  return(Val_sexp(Sexp_val(sexp)->u.promsxp.expr));
}


/**  Returns the environment of a promise.
  *
  *  @note This function bypasses the provided R API, and requires the
  *        use of the #define USE_RINTERNALS directive.
  *
  *  @param sexp An R promise.
  *  @return The environment of the R promise.
  */
CAMLprim value ocamlr_inspect_promsxp_env (value sexp) {
  return(Val_sexp(Sexp_val(sexp)->u.promsxp.env));
}


/**  Returns an element of a logical vector.
  *
  *  @param lglsxp An R logical vector, of sexptype LGLSXP.
  *  @param offset An integer, offset in the R logical vector.
  *  @return The boolean at this offset in the R logical vector.
  */
CAMLprim value ocamlr_access_lglsxp (value lglsxp, value offset) {
  return(Val_bool(LOGICAL(Sexp_val(lglsxp))[Int_val(offset)]));
}

CAMLprim value ocamlr_access_lglsxp_opt(value lglsxp, value offset) {
  int i = LOGICAL(Sexp_val(lglsxp))[Int_val(offset)];
  if(i == NA_LOGICAL) return Val_int(0);
  else {
    value some_i = caml_alloc(1, 0);
    Store_field(some_i, 0, Val_bool(i)) ;
    return(some_i);
  }
}


/**  Returns an element of a vector of integers.
  *
  *  @param intsxp An R vector of integers, of sexptype INTSXP.
  *  @param offset An integer, offset in the R vector of integers.
  *  @return The integer at this offset in the R vector of integers.
  */
CAMLprim value ocamlr_access_intsxp (value intsxp, value offset) {

  /* The R macro is #define INTEGER(x) ((int *) DATAPTR(x)).
     Should use Val_int, or int32s? More generally, the typing
     is here somewhat confusing (or confused)... Is offset an int? */

  return(Val_int(INTEGER(Sexp_val(intsxp))[Int_val(offset)]));
}

/**  Returns an element of a vector of integers, accounting for NAs
  *
  *  @param intsxp An R vector of integers, of sexptype INTSXP.
  *  @param offset An integer, offset in the R vector of real numbers.
  *  @return an int option, [None] if [NA] or [Some x] where [x] is integer at this offset in the R vector of real numbers.
  */
CAMLprim value ocamlr_access_intsxp_opt (value intsxp, value offset) {
  int x = INTEGER(Sexp_val(intsxp))[Int_val(offset)];
  if(x == NA_INTEGER) return Val_int(0);
  else {
    value some_x = caml_alloc(1, 0);
    Store_field(some_x, 0, Val_int(x)) ;
    return(some_x);
  }
}

/**  Returns an element of a vector of real numbers.
  *
  *  @param realsxp An R vector of real numbers.
  *  @param offset An integer, offset in the R vector of real numbers.
  *  @return The real number at this offset in the R vector of real numbers.
  */
CAMLprim value ocamlr_access_realsxp (value realsxp, value offset) {
  return(caml_copy_double(REAL(Sexp_val(realsxp))[Int_val(offset)]));
}


/**  Returns an element of a vector of real numbers, accounting for NAs
  *
  *  @param realsxp An R vector of real numbers.
  *  @param offset An integer, offset in the R vector of real numbers.
  *  @return a float option, [None] if [NA] or [Some x] where [x] is real number at this offset in the R vector of real numbers.
  */
CAMLprim value ocamlr_access_realsxp_opt(value realsxp, value offset) {
  double x = REAL(Sexp_val(realsxp))[Int_val(offset)];
  if(ISNA(x)) return Val_int(0);
  else {
    value some_x = caml_alloc(1, 0);
    Store_field(some_x, 0, caml_copy_double(x)) ;
    return(some_x);
  }
}

/**  Returns an element of a vector of strings.
  *
  *  @param strsxp An R vector of strings.
  *  @param offset An integer, offset in the R vector of strings.
  *  @return The string at this offset in the R vector of strings.
  */
CAMLprim value ocamlr_access_strsxp (value strsxp, value offset) {

  /* Same comments as for r_access_int_vecsxp and for
     r_internal_string_of_charsxp. */

  return(caml_copy_string(CHAR(STRING_ELT(Sexp_val(strsxp), (Int_val(offset))))));
}

CAMLprim value ocamlr_access_strsxp_opt (value strsxp, value offset) {
  SEXP s = STRING_ELT(Sexp_val(strsxp), Int_val(offset));
  if(s == NA_STRING) return Val_int(0);
  else {
    value some_s = caml_alloc(1,0);
    Store_field(some_s, 0, caml_copy_string(CHAR(s)));
    return(some_s);
  }
}

/**  Returns an element of a vector of SEXPs.
  *
  *  r_access_sexp_vecsxp takes a vector of SEXPs as argument,
  *  and an offset, and returns the element at this offset.
  */

/* This function could also be called r_access_expr_vecsxp. */
/**  Returns an element of a vector of SEXPs.
  *
  *  @note This function is also used to retrieve an element in
  *        a vector of expressions, of sexptype EXPRSXP.
  *
  *  @param sexpsxp An R vector of SEXPs.
  *  @param offset An integer, offset in the R vector of SEXPs.
  *  @return The SEXP at this offset in the R vector of SEXPs.
  */
CAMLprim value ocamlr_access_vecsxp(value sexpsxp, value offset) {
  return(Val_sexp(VECTOR_ELT(Sexp_val(sexpsxp), Int_val(offset))));
}
