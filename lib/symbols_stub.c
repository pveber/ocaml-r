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

/* Dealing with symbols and environments. */

CAMLprim value ocamlr_install (value symbol) {

  /* The install function is defined in names.c. It basically looks up
     a symbol in the symbol table. If not found, it creates an entry in
     the symbol table. This is therefore an operation that has side-effects,
     but entry points in libR.so which would allow to implement a side-
     effect-free equivalent are hidden. */

  /* Remark: explanation of the install function at
     https://stat.ethz.ch/pipermail/r-devel/2009-August/054494.html */

  return(Val_sexp(install(String_val(symbol))));
}

CAMLprim value ocamlr_findvar (value symbol) {
  /* The findVar function is defined in envir.c. It looks up a symbol
     in an environment. */
  return(Val_sexp(findVar(Sexp_val(symbol), R_GlobalEnv)));
}

CAMLprim value ocamlr_findfun (value symbol) {
  /* This is essentially the same as findvar, with dynamic type checking
     for function types built into the R source code. The major difference
     is that findvar yields a promise, whether findfun yiels directly
     an R function sexptype. */
  return(Val_sexp(findFun(Sexp_val(symbol), R_GlobalEnv)));
}

/**********************************************************************
 *                                                                    *
 *                 Old code concerning symbols.                       *
 *                                                                    *
 **********************************************************************/

//CAMLprim value r_set_var (value name, value sexp) {
//  CAMLparam2(name,sexp);
//  char* c_name = String_val(name);
//  SEXP e = (SEXP) Long_val(Field(sexp,0));
//  setVar(install(c_name), e, R_GlobalEnv);
//  CAMLreturn(Val_unit);
//}
