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

/* Memory allocation is done via functions exported from memory.c. */

/**  Allocates a pairlist.
  *
  *  @note With 'let x = f () in x::(aux y)', you have tail recursion
  *        because of the way the :: constructor is used in OCaml. You
  *        cannot have tail-recursion with R CONS and the same code.
  *        Hence the need to pre-allocate with this function.
  *  @param i Size of the pairlist to allocate.
  *  @return A newly allocated pairlist.
  */
CAMLprim value ocamlr_alloc_list (value i) {
  /* allocList is a macro wrapping up a call to the
     Rf_allocList symbol from libR.so. */
  return(Val_sexp(allocList(Int_val(i))));
}


/**  Allocates a logical vector.
  *
  *  @param i Size of the logical vector to allocate.
  *  @return A newly allocated logical vector.
  */
CAMLprim value ocamlr_alloc_lgl_vector (value i) {
  return(Val_sexp(allocVector(LGLSXP, Int_val(i))));
}


/**  Allocates a vector of integers.
  *
  *  @param i Size of the vector of integers to allocate.
  *  @return A newly allocated vector of integers.
  */
CAMLprim value ocamlr_alloc_int_vector (value i) {
  return(Val_sexp(allocVector(INTSXP, Int_val(i))));
}


/**  Allocates a vector of real numbers.
  *
  *  @param i Size of the vector of real numbers to allocate.
  *  @return A newly allocated vector of real numbers.
  */
CAMLprim value ocamlr_alloc_real_vector (value i) {
  return(Val_sexp(allocVector(REALSXP, Int_val(i))));
}


/**  Allocates a vector of strings.
  *
  *  @param i Size of the vector of strings to allocate.
  *  @return A newly allocated vector of strings.
  */
CAMLprim value ocamlr_alloc_str_vector (value i) {
  return(Val_sexp(allocVector(STRSXP, Int_val(i))));
}
