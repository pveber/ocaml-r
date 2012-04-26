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

external sexp_equality : sexp -> sexp -> bool = "ocamlr_sexp_equality"

(* R constants - global symbols in libR.so. *)
(* We are looking for a clean solution
   for the typing of the R NULL. What should it be
   in OCaml? An 'a option mapping to None? *)
external null_creator : unit -> nilsxp = "ocamlr_null"
external dots_symbol_creator : unit -> sexp = "ocamlr_dots_symbol"
external missing_arg_creator : unit -> sexp = "ocamlr_missing_arg"
external base_env_creator : unit -> sexp = "ocamlr_base_env"

(* R_GlobalEnv is not a constant, but rather a constant pointer,
   that gets updated by R itself. *)
external global_env : unit -> sexp = "ocamlr_global_env"
