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

(* TODO: We will have to use polymorphic variants and private
   type abreviations to leverage the typing system to its full
   extent to type R statically. *)

(* Types of wrapped R SEXP values. sxp is a polymorphic type
   wrapping up the monomorphic type sexp *)

(* Argument types for the polymorphic 'a sxp type. *)
(*type 'a sxp = sexp*)
(*type nil*)                         (* For NILSXP *)
(*type sym*)                         (* For SYMSXP *)
(*type 'a lisplist*)                 (* For LISTSXP, and LANGSXP *)
(*type simple*)                      (* For LISTSXP *)
(*type pairlist = simple lisplist*)  (* For LISTSXP *)
(*type clos*)                        (* For CLOSXP *)
(*type env*)                         (* For ENVSXP *)
(*type prom*)                        (* For PROMSXP *)
(*type call*)                        (* For LANGSXP *)
(*type lang = call lisplist*)        (* For LANGSXP *)
(*type builtin*)                     (* For BUILTINSXP *)
(* Phantom type vec, and phantom subtype vecsxp. *)
(*type 'a vec*)                      (* For all the VECSXPs *)
(*type 'a vecsxp = 'a vec sxp*)
(*type vec_char = char vec*)         (* For CHARSXP *)
(*type vec_lgl  = bool vec*)         (* For LGLSXP *)
(*type vec_int  = int  vec*)         (* For INTSXP *)
    (* Or shouldn't it be int32 vec ? *)
(*type vec_real = float vec*)        (* For REALSXP *)
(*type vec_str  = string vec*)       (* For STRSXP *)
(*type vec_sexp = sexp vec*)
(*type vec_expr = lang sxp vec*)     (* For EXPRSXP *)

open Data

(* Algebraic type reflecting R's dynamic typing. *)
type sexptype =
  | NilSxp
  | SymSxp
  | ListSxp
  | CloSxp
  | EnvSxp
  | PromSxp
  | LangSxp
  | SpecialSxp
  | BuiltinSxp
  | CharSxp
  | LglSxp
  | IntSxp
  | RealSxp
  | CplxSxp
  | StrSxp
  | DotSxp
  | AnySxp
  | VecSxp
  | ExprSxp
  | BcodeSxp
  | ExtptrSxp
  | WeakrefSxp
  | RawSxp
  | S4Sxp
  | FunSxp

external sexptype_of_sexp : sexp -> int = "ocamlr_sexptype_of_sexp" "noalloc"
let sexptype s = match (sexptype_of_sexp s) with
  | 0  -> NilSxp
  | 1  -> SymSxp
  | 2  -> ListSxp
  | 3  -> CloSxp
  | 4  -> EnvSxp
  | 5  -> PromSxp
  | 6  -> LangSxp
  | 7  -> SpecialSxp
  | 8  -> BuiltinSxp
  | 9  -> CharSxp
  | 10 -> LglSxp
    (* Integer range is not defined here. *)
  | 13 -> IntSxp
  | 14 -> RealSxp
  | 15 -> CplxSxp
  | 16 -> StrSxp
  | 17 -> DotSxp
  | 18 -> AnySxp
  | 19 -> VecSxp
  | 20 -> ExprSxp
  | 21 -> BcodeSxp
  | 22 -> ExtptrSxp
  | 23 -> WeakrefSxp
  | 24 -> RawSxp
  | 25 -> S4Sxp
  (* 99 represents a 'dummy' type for functions, with is an
     umbrella for Closure, Builtin or Special types. *)
  | 99 -> FunSxp
  | _ -> failwith "R value with type not specified in Rinternals.h"

let string_of_sexptype = function
  | NilSxp     -> "NilSxp"
  | SymSxp     -> "SymSxp"
  | ListSxp    -> "ListSxp"
  | CloSxp     -> "CloSxp"
  | EnvSxp     -> "EnvSxp"
  | PromSxp    -> "PromSxp"
  | LangSxp    -> "LangSxp"
  | SpecialSxp -> "SpecialSxp"
  | BuiltinSxp -> "BuiltinSxp"
  | CharSxp    -> "CharSxp"
  | LglSxp     -> "LglSxp"
  | IntSxp     -> "IntSxp"
  | RealSxp    -> "RealSxp"
  | CplxSxp    -> "CplxSxp"
  | StrSxp     -> "StrSxp"
  | DotSxp     -> "DotSxp"
  | AnySxp     -> "AnySxp"
  | VecSxp     -> "VecSxp"
  | ExprSxp    -> "ExprSxp"
  | BcodeSxp   -> "BcodeSxp"
  | ExtptrSxp  -> "ExtptrSxp"
  | WeakrefSxp -> "WeakrefSxp"
  | RawSxp     -> "RawSxp"
  | S4Sxp      -> "S4Sxp"
  | FunSxp     -> "FunSxp"

let is_function x = match sexptype x with
  | CloSxp | SpecialSxp | BuiltinSxp | FunSxp -> true
  | _ -> false
