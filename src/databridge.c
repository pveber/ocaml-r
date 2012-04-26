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

/* Wrapping and unwrapping of R values. */

/* TODO: Abstract some low level wrapping to a dedicated .so (i.e. other project). */

/* TODO: Think of replacing caml_alloc by caml_alloc_small for efficiency
   reasons. However, one has to deal with GC interaction. See the posts
   http://www.mail-archive.com/caml-list@yquem.inria.fr/msg00725.html and
   http://www.mail-archive.com/caml-list@yquem.inria.fr/msg00726.html */

/* TODO: There are some uses of malloc that should be replaced by
   caml_stat_alloc, which is malloc, but raises the OCaml out of memory
   exception. See http://old.nabble.com/help-with-caml_stat_alloc-tc5463112.html */

/* TODO: To wrap strings, (which we do elsewhere), or more precisely, to
   convert an OCaml string to a C string, INRIA guys use the construct

          char * p;
          p = caml_stat_alloc(caml_string_length(path) + 1);
          strcpy(p, String_val(path));
          [...]
          caml_stat_free(p);

   We should investigate this construct. */

/* TODO: We are now using custom blocks for pointer to R SEXPs. We use values of
   0, 1 in caml_alloc_custom to calibrate the GC. This is obviously insufficient,
   as a SEXP might hold a huge amount of data, specifically in R. We should
   therefore think of writing a BigVal_sexp(SEXP sexp, mlsize_t mem, mlsize_t max)
   function, allowing us to taylor the behaviour of the GC, depending on data size. */

/* TODO: Unfortunately, the scheme used to protect and unprotect values in R with
   ReleaseObject and PreserveObject is highly inefficient. The time to release an
   object is linear in the amount of object preserved. This is a huge runtime penalty
   to interface OCaml's garbage collector with R's garbage collector. Therefore, the
   only way forward is to create our own garbage collector for the interface within
   a global generic vector. Easy, or not? */

/* TODO: When executing an R function call, we let go off the OCaml multithreading
   master lock. Therefore, we should not have OCaml code going in Val_sexp, because
   that would trigger allocation, and GC stuff, while at the same time, R code might
   be executed, whose C code's underlying assumptions rely on not having garbage
   collection between two statements. So on should ensure that Val_sexp is not called
   in way that multithreads with R code. */



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

/**  Finalisation function called when OCaml's GC deallocates wrapped R values.
  *
  *  @note This tells R's GC to release this value.
  *  @param valsexp An OCaml value wrapping an R value.
  */
static void r_valsexp_finalisation (value valsexp) {
  R_ReleaseObject(*((SEXP *) Data_custom_val(valsexp)));
}


/**  Static structure used for custom blocks wrapping R values.
  */
static struct custom_operations r_sexp_ops = {
  "org.homelinux.yziquel.OCaml-R",
  r_valsexp_finalisation,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};


/**  Wraps up an R value into an OCaml block.
  *
  *  The R value is preserved from R garbage collection, then
  *  wrapped in an OCaml custom block, with finalisation function
  *  chaining OCaml's GC with R's GC.
  *
  *  @param sexp An R value.
  *  @return An R value wrapped in a OCaml block.
  */
CAMLprim value Val_sexp (SEXP sexp) {
  R_PreserveObject(sexp);  // registers the SEXP as an R GC root.
  value result = caml_alloc_custom(&r_sexp_ops, sizeof(SEXP), 0, 1);
  (*((SEXP *) Data_custom_val(result))) = sexp;
  return result;
}
