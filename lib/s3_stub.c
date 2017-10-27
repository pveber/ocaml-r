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

/**  Get the S3 class of a given SEXP.
  *
  *  r_s3_class takes a SEXP as argument, and returns the S3 class
  *  attribute of the given SEXP.
  *
  *  This calls getAttrib, which is part of the API, to access the
  *  "class" attribute. Usage is (weakly) documented in section
  *  5.9.4 "Attributes" of the "Writing R extensions" documentation,
  *  a.k.a. r-exts.pdf.
  *
  *  Note: Calls to getAttrib do a lot of unnecessary dynamic type
  *  checking. This will have to clarified and cleaned up in future
  *  versions of OCaml-R, possibly bypassing the API.
  */

CAMLprim value ocamlr_s3_class (value sexp) {
  return(Val_sexp(getAttrib(Sexp_val(sexp), R_ClassSymbol)));
}


/**  Get an attribute of a given SEXP.
  *
  *  r_get_attrib takes a SEXP as first argument, then an R symbol
  *  name, and returns the attribute of the first argument matching
  *  this symbol name. Such symbols can be created with the install
  *  function, from Objective Caml side.
  *
  *  Note: As above, lots a dynamic type checking that will have to
  *  be bypassed.
  */

CAMLprim value ocamlr_get_attrib (value sexp, value symbolname) {
  return(Val_sexp(getAttrib(Sexp_val(sexp), Sexp_val(symbolname))));
}


/**  Get the attribute list of a given SEXP.
  *
  *  r_get_attributes takes a SEXP as first argument, and returns
  *  the attributes of the given SEXP.
  */

CAMLprim value ocamlr_get_attributes (value sexp) {
  return(Val_sexp(ATTRIB(Sexp_val(sexp))));
}
