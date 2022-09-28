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

/* The NULL constant, and others, in R... */

CAMLprim value ocamlr_null (value unit) {
  return Val_sexp(R_NilValue);
}

CAMLprim value ocamlr_dots_symbol (value unit) {
  return Val_sexp(R_DotsSymbol);
}

CAMLprim value ocamlr_missing_arg (value unit) {
  return Val_sexp(R_MissingArg);
}

CAMLprim value ocamlr_base_env (value unit) {
  return Val_sexp(R_BaseEnv);
}

CAMLprim value ocamlr_global_env (value unit) {
  return Val_sexp(R_GlobalEnv);
}


/* Comparison operator. */
CAMLprim value ocamlr_sexp_equality (value s1, value s2) {
  return Val_bool(Sexp_val(s1) == Sexp_val(s2));
}


/* Extracting runtime R low-level type information. */

CAMLprim value ocamlr_sexptype_of_sexp (value sexp) {
  return Val_int(TYPEOF(Sexp_val(sexp)));
}

/* Printing an SEXP */
CAMLprim value ocamlr_print_value(value sexp) {
  Rf_PrintValue(Sexp_val(sexp));
  return Val_unit;
}

CAMLprim value is_missing_arg(value sexp) {
  return Val_bool(Sexp_val(sexp) == R_MissingArg);
}
