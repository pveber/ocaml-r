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
open Parser
open Conversion

(* The following exception needs to be registered
   in a callback when the R interpreter is initialised. *)
exception Runtime_error of langsxp * string

external eval_langsxp : langsxp -> 'a t = "ocamlr_eval_sxp"

let eval_string s = eval_langsxp (parse s)
(* TODO: May segfault if stumbling on a symbol that
 * hasn't yet been loaded. *)

let rec prepare_args = function
  | (Some x)::l -> x::(prepare_args l)
  | None::l     -> prepare_args l
  | []          -> []

let arg f ?name x = Some (name, (Obj.magic (f x)))
let opt f name x = match x with
  | None -> None
  | Some x -> Some ((Some name), (Obj.magic (f x)))

let eval phi (args: (string option * sexp) option list) =
  eval_langsxp (langsxp phi (prepare_args args))
