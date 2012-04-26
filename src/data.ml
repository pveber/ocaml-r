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
type +'a sxp = sexp constraint 'a =
  [< `Nil
  |  `Sym
  |  `List of [< `Pair | `Call ]
  |  `Clo
  |  `Env
  |  `Prom
  |  `Special
  |  `Builtin
  |  `Vec of [< `Char | `Lgl | `Int | `Real | `Str | `Raw | `Expr ]
  ]
type +'a t = sexp

external cast_to_sxp : sexp -> 'a sxp = "%identity"
external cast : sexp -> 'a t = "%identity"


(* Type aliases. *)

type nilsxp         = [`Nil]                                      sxp
type symsxp         = [`Sym]                                      sxp
type 'a listsxp     = [`List of [< `Pair | `Call ] as 'a]         sxp
and 'a internallist = [`Nil | `List of [< `Pair | `Call ] as 'a ] sxp  (**  Type of low-level internal list. In R, such
                                                                         *  internal lists may be empty, a pairlist or
                                                                         *  a call which is somewhat similar to closure
                                                                         *  ready for execution. *)
type langsxp        = [`List of [`Call]]                          sxp
type pairlistsxp    = [`List of [`Pair]]                          sxp
and pairlist        = [`Nil | `List of [`Pair]]                   sxp
type closxp         = [`Clo]                                      sxp
type envsxp         = [`Env]                                      sxp
type promsxp        = [`Prom]                                     sxp
type specialsxp     = [`Special]                                  sxp
type builtinsxp     = [`Builtin]                                  sxp
type 'a vecsxp      = [`Vec  of
    [< `Char | `Lgl | `Int  | `Real
    | `Str  | `Raw | `Expr ] as 'a
  ] sxp
type charvecsxp  = [`Vec  of [`Char]]                             sxp
type lglvecsxp   = [`Vec  of [`Lgl ]]                             sxp
type intvecsxp   = [`Vec  of [`Int ]]                             sxp
type realvecsxp  = [`Vec  of [`Real]]                             sxp
type strvecsxp   = [`Vec  of [`Str ]]                             sxp
type rawvecsxp   = [`Vec  of [`Raw ]]                             sxp
type exprvecsxp  = [`Vec  of [`Raw ]]                             sxp

