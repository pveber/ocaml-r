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
open Sexptype
open Promise

(* There's a lot of stuff concerning symbols and environments in the
   envir.c file of the R source code. *)

external install : string -> symsxp = "ocamlr_install"

external findvar : symsxp -> promsxp = "ocamlr_findvar"

external findfun : symsxp -> promsxp = "ocamlr_findfun"

let symbol ?(generic = false) s : sexp =

  let findfunction = match generic with
    | false -> findvar | true -> findfun in

  let var = force_promsxp (findfunction (install s)) in

  (* If we try to retrieve a function, we should use findfun. If we
     use findvar, we indeed get a closure, but a closure for a generic
     function in the sense of R objects:

     CLOSURE {formals = LIST [(ARG "object", PLACE); (ARG "...", PLACE)];

     and you get runtime errors. This is why we have a dynamic type check
     here, and this is why this symbol should be used as little as
     possible at R runtime. *)

  match is_function var with
  | false -> var
  | true -> force_promsxp (findfun (install s))
