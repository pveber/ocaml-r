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

/* Parsing R code. */


/**  Parses a given string.
  *
  *  @param expression A string containing R code, textually.
  *  @param n_to_eval How many calls to parse. If set to -1, stops
  *         when the string to parse is parsed completely.
  *  @return An integer error code, and an R vector, of type EXPRSXP,
  *          containing the parsing result.
  */
CAMLprim value ocamlr_parse_string (value expression, value n_to_eval) {
  CAMLparam1(expression);
  CAMLlocal1(parse_result);
  ParseStatus status;
  SEXP text;
  PROTECT(text = mkString(String_val(expression)));

  /* Concerning the last and fourth argument of R_ParseVector:
     http://tolstoy.newcastle.edu.au/R/e2/devel/07/01/1835.html */

  parse_result = Val_sexp(R_ParseVector(text, Int_val(n_to_eval), &status, R_NilValue));
  UNPROTECT(1);
  value result = caml_alloc(2, 0);
  Store_field(result, 0, Val_int(status));
  Store_field(result, 1, parse_result);
  CAMLreturn(result);
}
