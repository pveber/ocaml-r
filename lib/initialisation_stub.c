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

/* Stub code initialising and terminating the R interpreter. */

/**  Initialises the R interpreter in the libR.so library.
  *
  *  @param argv An OCaml array of strings, which gives the command
  *         line arguments used to invoke the R interpreter. Code
  *         segfaults if the array does not contain a first element,
  *         which is the name of the program, typically "R", or
  *         "OCaml-R". Other arguments typically are "--vanilla",
  *         "--slave"...
  *  @param sigs An OCaml integer. When set to 0, R signal handlers
  *         are not removed. When set, for example, to 1, R signal
  *         handlers are removed. It is very useful to remove signal
  *         handlers when embedding R. Requires R >= 2.3.1.
  *  @return 1 if R is correctly initialised.
  */
CAMLprim value ocamlr_initEmbeddedR (value ml_argv, value ml_sigs) {

  int length = Wosize_val(ml_argv);
  char* argv[length];
  int i;

  // We duplicate the OCaml array into a C array.
  for (i=0; i<length; i++) argv[i] = (char*) Bytes_val(Field(ml_argv, i));

  /* Don't let R set up its own signal handlers when sigs = 1.
     This requires R >= 2.3.1. */
  if (Int_val(ml_sigs)) R_SignalHandlers = 0;

  // This is the libR.so function.
  i = Rf_initEmbeddedR(length, argv);

  // Returns 1 if R is correctly initialised.
  return Val_int(i);
}


/**  Terminates an R interpreter. */
CAMLprim value ocamlr_endEmbeddedR (value unit) {
  /* This function terminates the R interpreter. It is not clear whether
     or not this function is garbage-collector-friendly. For details, see
     http://old.nabble.com/Reset-an-embedded-R.dll-td17236931.html */
  Rf_endEmbeddedR(0);
  return Val_unit;
}

