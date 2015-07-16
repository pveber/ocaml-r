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

(* The type system of OCaml-R. *)

type sexp

type +'a t = sexp

external cast : sexp -> 'a t = "%identity"


(* Type aliases. *)

type nilsxp         = [`Nil]                                      t
type symsxp         = [`Sym]                                      t
type 'a listsxp     = [`List of [< `Pair | `Call ] as 'a]         t
and 'a internallist = [`Nil | `List of [< `Pair | `Call ] as 'a ] t    (**  Type of low-level internal list. In R, such
                                                                         *  internal lists may be empty, a pairlist or
                                                                         *  a call which is somewhat similar to closure
                                                                         *  ready for execution. *)
type langsxp        = [`List of [`Call]]                          t
type pairlistsxp    = [`List of [`Pair]]                          t
and pairlist        = [`Nil | `List of [`Pair]]                   t
type closxp         = [`Clo]                                      t
type envsxp         = [`Env]                                      t
type promsxp        = [`Prom]                                     t
type specialsxp     = [`Special]                                  t
type builtinsxp     = [`Builtin]                                  t
type 'a vecsxp      = [`Vec  of
    [< `Char | `Lgl | `Int  | `Real
    | `Str  | `Raw | `Expr ] as 'a
  ] t
type charvecsxp  = [`Vec  of [`Char]]                             t
type lglvecsxp   = [`Vec  of [`Lgl ]]                             t
type intvecsxp   = [`Vec  of [`Int ]]                             t
type realvecsxp  = [`Vec  of [`Real]]                             t
type strvecsxp   = [`Vec  of [`Str ]]                             t
type rawvecsxp   = [`Vec  of [`Raw ]]                             t
type exprvecsxp  = [`Vec  of [`Raw ]]                             t



class type ['a] ty = object
  method repr : 'a
end
(** *)

type _ scalar_format =
  | Integer : int scalar_format
  | Real : float scalar_format
  | Logical : bool scalar_format
  | String : string scalar_format

(*
  The following slightly contrived definition are needed to workaround
  the compiler, see
  https://groups.google.com/forum/#!topic/fa.caml/-7yCBMx7xxw
*)
class type ['a, 'int] atomic_vector0 = object
  inherit ['a list] ty
  method length : 'int
end

class type ['a, 'int] scalar0 = object
  inherit ['a, 'int] atomic_vector0
  method scalar : unit
end

class type ['a] scalar = object
  inherit ['a, (int, int) scalar0] scalar0
end

class type ['a] atomic_vector = object
  inherit ['a, int scalar] atomic_vector0
end

class type reals = object
  inherit [float] atomic_vector
end

class type real = object
  inherit [float] scalar
end

class type integers = object
  inherit [int] atomic_vector
end

class type integer = object
  inherit [int] scalar
end

class type strings = object
  inherit [string] atomic_vector
end

class type string_ = object
  inherit [string] scalar
end

class type logicals = object
  inherit [bool] atomic_vector
end

class type logical = object
  inherit [bool] scalar
end

class type ['a] s3 = object
  inherit ['a] ty
  method classes : string list
end

class type ['a] list_ = object
  inherit ['a] s3 constraint 'a = < .. >
  method ty : 'a
  method length : int
  method subset2_s : 'b. string -> 'b
  method subset2_i : 'b. string -> 'b
end

class type ['a] data'frame  = object
  inherit ['a] list_
  method dim : int * int
end
