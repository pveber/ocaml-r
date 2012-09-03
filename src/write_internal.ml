(*********************************************************************************)
(*                OCaml-R                                                        *)
(*                                                                               *)
(*    Copyright (C) 2008-2010 Institut National de Recherche en                  *)
(*    Informatique et en Automatique. All rights reserved.                       *)
(*                                                                               *)
(*    Copyright (C) 2009-2010 Guillaume Yziquel. All rights reserved.            *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation; either version 3 of the         *)
(*    License, or  any later version.                                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public                  *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*             guillaume.yziquel@citycable.ch                                    *)
(*********************************************************************************)

open Data

external write_listsxp_carval : 'a listsxp -> sexp -> unit = "ocamlr_write_lisplist_carval"
external write_listsxp_tagval : 'a listsxp -> sexp -> unit = "ocamlr_write_lisplist_tagval"

let write_listsxp_element l tag elmnt =
  let () = write_listsxp_tagval l tag in
  let () = write_listsxp_carval l elmnt in
  ()

(**  Sets the element of a logical vector.
  *
  *  assign_lgl_vecsxp takes a logical vector as first argument,
  *  an offset as second argument, and a boolean as third argument,
  *  and sets the vector's offset element to the boolean's value.
  *)

external assign_lglvecsxp  : lglvecsxp -> int -> bool -> unit = "ocamlr_assign_lglvecsxp"


(**  Sets the element of a vector of integers.
  *
  *  assign_int_vecsxp takes a vector of integers as first argument,
  *  an offset as second argument, and an integer as third argument,
  *  and sets the vector's offset element to the integer's value.
  *
  *  Question: should we rather map R's integers to int32s?
  *)

external assign_intvecsxp  : intvecsxp -> int -> int -> unit = "ocamlr_assign_intvecsxp"


(**  Sets the element of a vector of real numbers.
  *
  *  assign_real_vecsxp takes a vector of real numbers as first argument,
  *  an offset as second argument, and a real number as third argument,
  *  and sets the vector's offset element to the real number's value.
  *)

external assign_realvecsxp : realvecsxp -> int -> float -> unit = "ocamlr_assign_realvecsxp"


(**  Sets the element of a vector of real numbers with possibly missing values
  *
  *  assign_real_vecsxp takes a vector of real numbers as first argument,
  *  an offset as second argument, and a possibly missig real number as third argument,
  *  and sets the vector's offset element to the real number's value or NA if non available.
  *)

external assign_realvecsxp_opt : realvecsxp -> int -> float option -> unit = "ocamlr_assign_realvecsxp_opt"


(**  Sets the element of a vector of string.
  *
  *  assign_str_vecsxp takes a vector of strings as first argument,
  *  an offset as second argument, and a string as third argument,
  *  and sets the vector's offset element to the string's value.
  *)

external assign_strvecsxp  : strvecsxp -> int -> string -> unit = "ocamlr_assign_strvecsxp"
