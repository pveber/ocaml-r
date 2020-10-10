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

CAMLprim value ocamlr_write_lisplist_carval (value lisplist, value elmnt) {
  Sexp_val(lisplist)->u.listsxp.carval = Sexp_val(elmnt);
  return Val_unit;
}

CAMLprim value ocamlr_write_lisplist_tagval (value lisplist, value tag) {
  Sexp_val(lisplist)->u.listsxp.tagval = Sexp_val(tag);
  return Val_unit;
}

//CAMLprim value r_write_lisplist_element (value lisplist, value tag, value elmnt) {
//  CAMLparam3(lisplist, tag, elmnt);
//  Sexp_val(lisplist)->u.listsxp.tagval = Sexp_val(tag);
//  Sexp_val(lisplist)->u.listsxp.carval = Sexp_val(elmnt);
//  CAMLreturn(Val_unit);
//}


/**  Sets the element of a logical vector.
  *
  *  ocamlr_assign_lgl_vecsxp takes a logical vector as first argument,
  *  an offset as second argument, and a boolean as third argument,
  *  and sets the vector's offset element to the boolean's value.
  */

CAMLprim value ocamlr_assign_lglsxp (value lglsxp, value offset, value b) {
  LOGICAL(Sexp_val(lglsxp))[Int_val(offset)] = Bool_val(b);
  return Val_unit;
}

CAMLprim value ocamlr_assign_lglsxp_opt (value lglsxp, value offset, value b) {
  int rl = (b == Val_int(0)) ? NA_LOGICAL : Bool_val(Field(b,0)) ;
  LOGICAL(Sexp_val(lglsxp))[Int_val(offset)] = rl;
  return Val_unit;
}

/**  Sets the element of a vector of integers.
  *
  *  ocamlr_assign_int_vecsxp takes a vector of integers as first argument,
  *  an offset as second argument, and an integer as third argument,
  *  and sets the vector's offset element to the integer's value.
  *
  *  Question: should we rather map R's integers to int32s?
  */

CAMLprim value ocamlr_assign_intsxp (value intsxp, value offset, value i) {
  INTEGER(Sexp_val(intsxp))[Int_val(offset)] = Int_val(i);
  return Val_unit;
}

/**  
 * Sets the element of a vector of integer numbers with possibly
 * missing values.
 *
 * ocamlr_assign_intvecsxp_opt takes a vector of integer numbers as
 * first argument, an offset as second argument, and a possibly
 * missing integer number as third argument, and sets the vector's
 * offset element to the real number's value, or NA if not available.
 */

CAMLprim value ocamlr_assign_intsxp_opt (value intsxp, value offset, value io) {
  int ri = (io == Val_int(0)) ? NA_INTEGER : Int_val(Field(io,0)) ;
    
  INTEGER(Sexp_val(intsxp))[Int_val(offset)] = ri;
  return Val_unit;
}


/**  Sets the element of a vector of real numbers.
  *
  *  ocamlr_assign_real_vecsxp takes a vector of real numbers as first argument,
  *  an offset as second argument, and a real number as third argument,
  *  and sets the vector's offset element to the real number's value.
  */

CAMLprim value ocamlr_assign_realsxp (value realsxp, value offset, value x) {
  REAL(Sexp_val(realsxp))[Int_val(offset)] = Double_val(x);
  return Val_unit;
}


/**  Sets the element of a vector of real numbers with possibly missing values.
  *
  *  ocamlr_assign_real_vecsxp takes a vector of real numbers as first argument,
  *  an offset as second argument, and a possibly missing real number as third argument,
  *  and sets the vector's offset element to the real number's value, or NA if not 
  *  available.
  */

CAMLprim value ocamlr_assign_realsxp_opt (value realsxp, value offset, value x) {
  double rx = (x == Val_int(0)) ? NA_REAL : Double_val(Field(x,0)) ;
    
  REAL(Sexp_val(realsxp))[Int_val(offset)] = rx;
  return Val_unit;
}


/**  Sets the element of a vector of string.
  *
  *  ocamlr_assign_str_vecsxp takes a vector of strings as first argument,
  *  an offset as second argument, and a string as third argument,
  *  and sets the vector's offset element to the string's value.
  */

CAMLprim value ocamlr_assign_strsxp (value strsxp, value offset, value s) {
  STRING_PTR(Sexp_val(strsxp))[Int_val(offset)] = mkChar(String_val(s));
  return Val_unit;
}

CAMLprim value ocamlr_assign_strsxp_opt(value strsxp, value offset, value s) {
  SEXP rs = (s == Val_int(0)) ? NA_STRING : mkChar(String_val(Field(s, 0)));
  SET_STRING_ELT(Sexp_val(strsxp), Int_val(offset), rs);
  return Val_unit;
}

CAMLprim value ocamlr_assign_vecsxp (value vecsxp, value offset, value sexp) {
  SET_VECTOR_ELT(Sexp_val(vecsxp), Int_val(offset), Sexp_val(sexp));
  return Val_unit;
}
