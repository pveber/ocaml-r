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

(* Functions to initialise and terminate the R interpreter. *)

open Reduction
open Data
open Sexprec
open Environment

external initialise : string array -> int -> int = "ocamlr_initEmbeddedR" "noalloc"
external terminate : unit -> unit = "ocamlr_endEmbeddedR" "noalloc"

external init_error_hook : unit -> unit = "ocamlr_init_error_hook" "noalloc"

exception Initialisation_failed

let init ?(name     = try Sys.argv.(0) with _ -> "OCaml-R")
         ?(argv     = try List.tl (Array.to_list Sys.argv) with _ -> [])
         ?(env      = Standard.env)
         ?(packages = None)
         ?(sigs     = Standard.signal_handlers) () =
  let env_vars = begin match packages with
    | None -> env
    | Some [] -> ("R_DEFAULT_PACKAGES", "NULL")::env
    | Some libs -> ("R_DEFAULT_PACKAGES", (String.concat ", " libs))::env
    end in
  List.iter (function name, value -> Unix.putenv name value) env_vars;
  let r_sigs = match sigs with true -> 0 | false -> 1 in
  match initialise (Array.of_list (name::argv)) r_sigs with
  | 1 -> let () = Callback.register_exception "OCaml-R generic error"
           (* The Runtime_error is initialised with a nilsxp casted to a langsxp.
              This is ugly, but not unsafe. *)
           (Runtime_error ((cast_to_sxp ((null_creator ()) : nilsxp :> sexp ) : langsxp ), "")) in init_error_hook ()
  | _ -> raise Initialisation_failed

module type Interpreter = sig end

module Interpreter (Env : Environment) : Interpreter = struct

  let () = init ~name: Env.name
                ~argv: Env.options
                ~env:  Env.env
                ~packages: Env.packages
                ~sigs: Env.signal_handlers
                ()

  let () = at_exit terminate

end

