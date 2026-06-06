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

/* Data conversion to and from OCaml and R. */


/**  Cons operation in R.
  *
  *  @param car The head element.
  *  @param tail The tail list.
  *  @return The result of consing car and tail.
  */
CAMLprim value ocamlr_cons (value car, value tail) {
  return(Val_sexp(CONS(Sexp_val(car), Sexp_val(tail))));
}

/**  Cons operation in R for language pairlists
  *
  *  @param car The head element.
  *  @param tail The tail list.
  *  @return The result of consing car and tail.
  */
CAMLprim value ocamlr_lcons (value car, value tail) {
  return(Val_sexp(LCONS(Sexp_val(car), Sexp_val(tail))));
}


/**  Tagging a pairlist's head
  *
  *  @note This function is specifically used when constructing
  *        a LANGSXP for evaluation with named arguments.
  *
  *  @param s The pairlist.
  *  @param t The tag, as a string.
  */
CAMLprim value ocamlr_tag (value s, value t) {
  SET_TAG(Sexp_val(s), install(String_val(t)));
  return Val_unit;
}
