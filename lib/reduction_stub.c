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

/* TODO: declare static what should be declared static... */

/* TODO: fix memory leaks... */


/**********************************************************************
 *                                                                    *
 *                   Beta reduction of R calls.                       *
 *                                                                    *
 **********************************************************************/

CAMLprim value ocamlr_eval_sxp (value sexp_list) {

  /* sexp_list is an OCaml value containing a SEXP of sexptype LANGSXP.
     This is a LISP-style pairlist of SEXP values. r_eval_sxp executes
     the whole pairlist, and sends back the resulting SEXP wrapped up in
     an OCaml value. There's also an error handling mechanism. */

  /* r_eval_sxp handles values of type LANGSXP and PROMSXP. So we have two
     functions on the OCaml side associated to this stub, the first on
     with type lang sexp -> raw sexp, the other one with type
     prom sexp -> raw sexp. This also means that there is a dynamic type
     checking being done in the scope of the R_tryEval function, and it
     would be nice to shortcut it with statically typed equivalents. */

  CAMLparam1(sexp_list);
  CAMLlocalN(error_arguments,2);

  SEXP e;        // Placeholder for the result of beta-reduction.
  int error = 0; // Error catcher boolean.

  SEXP our_call = Sexp_val(sexp_list);
  caml_enter_blocking_section();
  e = R_tryEval(our_call, R_GlobalEnv, &error);
  caml_leave_blocking_section();

  /* Implements error handling from R to Objective Caml. */
  if (error) {
    error_arguments[0] = sexp_list;
    error_arguments[1] = caml_copy_string(R_curErrorBuf());

    /* The exception callback mechanism is described on the webpage
       http://www.pps.jussieu.fr/Livres/ora/DA-OCAML/book-ora118.html
       We should check to see if we could avoid the string-name lookup
       to avoid unnecessary delays in exception handling. */

    caml_raise_with_args(*caml_named_value("OCaml-R generic error"), 2, error_arguments);
  }

  CAMLreturn(Val_sexp(e));
}



/**********************************************************************
 *                                                                    *
 *                  Execution of R expressions.                       *
 *                                                                    *
 **********************************************************************/

/* The function below has been commented, because it should be a
   combination of a parsing function and of an eval function. */

//CAMLprim value r_sexp_of_string (value expression) {
//
//  /* This function makes use of the camlrtmp symbol. We'd like
//     to create a function with similar semantics which does not
//     populate the symbols table. */
//
//  CAMLparam1(expression);
//  char* c_name = "camlrtmp";
//  char* s_exp;
//  CAMLlocal1(result);
//  SEXP e, tmp;
//  int hadError;
//  ParseStatus status;
//
//  asprintf(&s_exp, "%s = %s", c_name, String_val(expression));
//  PROTECT(tmp = mkString(s_exp));
//  PROTECT(e = R_ParseVector(tmp, 1, &status, R_NilValue));
//  /* PrintValue(e); DEBUG */
//  R_tryEval(VECTOR_ELT(e,0), R_GlobalEnv, &hadError);
//  UNPROTECT(2);
//  free(s_exp);
//  result = r_sexp_of_symbol(caml_copy_string(c_name));
//  CAMLreturn(result);
//}
