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

CAMLprim value Val_sexp (SEXP sexp);

/**  Macro accessing the underlying SEXP wrapped in an OCaml block.
  *
  *  @param sexp An OCaml block wrapping up an R value.
  *  @return The underlying R value.
  */
#define Sexp_val(sexp) (*((SEXP *) Data_custom_val(sexp)))


/**  Macro wrapping up a R vector SEXP into an OCaml custom block.
  *
  *  @param x An R vector SEXP.
  *  @return An OCaml block wrapping up the R vector SEXP.
  */
#define Val_vecsexp(x) Val_sexp(x)


/**  Macro accessing the underlying vector SEXP in an OCaml block.
  *
  *  @param x An OCaml block wrapping up an R vector value.
  *  @return The underlying vector SEXP.
  */
#define Vecsexp_val(x) ((VECSEXP) Sexp_val(x))
