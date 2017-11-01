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


(* === ENVIRONMENT ===== *)

module type Environment =
sig

  val name : string    (* This is the first argument of argv for R.
                          Mandatory, otherwise libR.so segfaults. *)

  (* See R reference manual, refman.pdf, page 452, section intitled
     'Startup - Initialization at Start of an R Session' for details
     about the most important command line options.
     More options are documented on the following webpage:
     http://cran.r-project.org/doc/manuals/R-intro.html#Invoking-R *)

  val options : string list

  (* signal_handlers, if set to false, asks R not to install its
     signal handlers. I've been experiencing weird issues with R signal
     handlers, since, for instance, a SIGSEGV originating from OCaml is
     caught by libR.so, and R asks then asks whether or not you want to
     save your workspace, et ceterae. By default, set to false. *)
  val signal_handlers : bool

  val env : (string * string) list

  val packages : string list option

end

module Standard = Standard

(* === SEXPTYPE ===== *)


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
  inherit ['a array] ty
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


(* === DATA ===== *)


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

external sexptype_of_sexp : sexp -> int = "ocamlr_sexptype_of_sexp" [@@noalloc]

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


(* === PROMISE ===== *)

external force_promsxp : promsxp -> sexp = "ocamlr_eval_sxp"

(*let force : 'a promise -> 'a t = force_promsxp*)

(* For lazy evaluation, we have an issue here: R promises
   are recursively forced by eval. This means that the
   OCaml type system would be broken, because we would need
   to have 'a Lazy.t R.t = 'a Lazy.t Lazy.t R.t. There's two
   solutions:

   -1- 'a R.t would denote a R value of type 'a, lazy or not.
       This is suboptimal, because the OCaml type system could
       and should express these lazy semantics.

   -2- Make a dynamic check on the nature of the argument of
       the force function. If it is a lazy lazy value, we
       should force it manually, with OCaml semantics. If not,
       we can run eval on it. *)



(* === SYMBOL ===== *)

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


(* === READ_INTERNAL ===== *)

(* Low-level data manipulation functions. *)

(* What follows is low-level accessor functions, in order to inspect
   in details the contents of SEXPs and VECSEXPs. *)

external inspect_attributes : sexp   -> sexp = "ocamlr_inspect_attributes"
external length_of_vecsxp   : 'a vecsxp -> int  = "ocamlr_inspect_vecsxp_length"

external inspect_primsxp_offset  : [< `Special | `Builtin ] t -> int = "ocamlr_inspect_primsxp_offset"
external inspect_symsxp_pname    : symsxp         -> sexp          = "ocamlr_inspect_symsxp_pname"
external inspect_symsxp_value    : symsxp         -> sexp          = "ocamlr_inspect_symsxp_value"
external inspect_symsxp_internal : symsxp         -> sexp          = "ocamlr_inspect_symsxp_internal"
external inspect_listsxp_carval  : 'a listsxp     -> sexp          = "ocamlr_inspect_listsxp_carval"
external inspect_listsxp_cdrval  : 'a listsxp     -> sexp          = "ocamlr_inspect_listsxp_cdrval"
external inspect_listsxp_tagval  : 'a listsxp     -> sexp          = "ocamlr_inspect_listsxp_tagval"
external inspect_envsxp_frame    : envsxp         -> sexp          = "ocamlr_inspect_envsxp_frame"
external inspect_envsxp_enclos   : envsxp         -> sexp          = "ocamlr_inspect_envsxp_enclos"
external inspect_envsxp_hashtab  : envsxp         -> sexp          = "ocamlr_inspect_envsxp_hashtab"
external inspect_closxp_formals  : closxp         -> sexp          = "ocamlr_inspect_closxp_formals"
external inspect_closxp_body     : closxp         -> sexp          = "ocamlr_inspect_closxp_body"
external inspect_closxp_env      : closxp         -> sexp          = "ocamlr_inspect_closxp_env"
external inspect_promsxp_value   : promsxp        -> sexp          = "ocamlr_inspect_promsxp_value"
external inspect_promsxp_expr    : promsxp        -> sexp          = "ocamlr_inspect_promsxp_expr"
external inspect_promsxp_env     : promsxp        -> sexp          = "ocamlr_inspect_promsxp_env"

external access_lglvecsxp  : lglvecsxp  -> int -> bool     = "ocamlr_access_lgl_vecsxp"
external access_intvecsxp  : intvecsxp  -> int -> int      = "ocamlr_access_int_vecsxp"
external access_realvecsxp : realvecsxp -> int -> float    = "ocamlr_access_real_vecsxp"
external access_strvecsxp  : strvecsxp  -> int -> string   = "ocamlr_access_str_vecsxp"
external access_rawvecsxp  : rawvecsxp  -> int -> sexp     = "ocamlr_access_sexp_vecsxp"
external access_exprvecsxp : exprvecsxp -> int -> langsxp  = "ocamlr_access_sexp_vecsxp"

(* === ALLOCATION ===== *)

(* This file contains wrappers around allocation functions,
   returning uninitialised pairlists and vectors. *)

external alloc_list        : int -> 'a internallist = "ocamlr_alloc_list"
external alloc_lgl_vector  : int -> lglvecsxp       = "ocamlr_alloc_lgl_vector"
external alloc_int_vector  : int -> intvecsxp       = "ocamlr_alloc_int_vector"
external alloc_real_vector : int -> realvecsxp      = "ocamlr_alloc_real_vector"
external alloc_str_vector  : int -> strvecsxp       = "ocamlr_alloc_str_vector"

(* === WRITE_INTERNAL ===== *)


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

(* === SEXPREC ===== *)

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


(* === CONVERSION  ===== *)

let rec list_of_pairlist (ll : 'a internallist) =
  match sexptype (ll : 'a internallist :> sexp) with
  | NilSxp -> [] | ListSxp | LangSxp | DotSxp ->
  (* There's a typing issue with the DotSxp sexptype... TODO *)
  let ll : 'a listsxp = cast (ll : 'a internallist :> sexp) in
  ( (cast (inspect_listsxp_tagval ll) : symsxp (* TODO: This may be excessive *)), (inspect_listsxp_carval ll))
  :: (list_of_pairlist (cast (inspect_listsxp_cdrval ll) : pairlist))
  | _ -> failwith "Conversion failure in list_of_listsxp."

let pairlist_of_list (l: (sexp * sexp) list) =
  let r_l = alloc_list (List.length l) in
  let cursor = ref r_l in List.iter
  begin function (tag, value) ->
    let () = write_listsxp_element (cast (!cursor : pairlist :> sexp) : pairlistsxp) tag value in
    cursor := (cast (inspect_listsxp_cdrval (cast (!cursor : pairlist :> sexp) : pairlistsxp)) : pairlist)
  end l; r_l

external cons : sexp -> pairlist -> pairlistsxp = "ocamlr_cons"
external tag : pairlistsxp -> string -> unit = "ocamlr_tag"
external set_langsxp : pairlistsxp -> unit = "ocamlr_set_langsxp"

let langsxp (f: sexp) (args: (string option * sexp) list) : langsxp =
  let lcons hd tl = let x = cons hd tl in set_langsxp x; (cast (x : pairlistsxp :> sexp) : langsxp) in
  lcons f begin List.fold_right begin fun (t, hd) tl ->
    let x = cons hd tl in match t with
    | None -> (cast (x : pairlistsxp :> sexp) : pairlist)
    | Some name -> tag x name; (cast (x : pairlistsxp :> sexp) : pairlist)
  end args ((null_creator ()) : nilsxp :> pairlist) end

external string_of_charsxp : charvecsxp -> string = "ocamlr_internal_string_of_charsxp"

let list_of_vecsxp (access : 'a vecsxp -> int -> 'b) (s : 'a vecsxp) =
  let lngth = length_of_vecsxp s in
  let rec aux n accu = match n with | 0 -> accu | _ ->
    let x = access s (n - 1) in aux (n - 1) (x :: accu)
  in aux lngth []

let vecsxp_of_list (alloc : int -> 'a vecsxp) (assign : 'a vecsxp -> int -> 'b -> unit) (l: 'b list) =
  let s = alloc (List.length l) in
  let rec aux offset = function | [] -> () | hd::tl ->
    let () = assign s offset hd in aux (1 + offset) tl
  in aux 0 l; s

let array_of_vecsxp (access : 'a vecsxp -> int -> 'b) (s : 'a vecsxp) =
  let lngth = length_of_vecsxp s in
  Array.init lngth (access s)

let vecsxp_of_array (alloc : int -> 'a vecsxp) (assign : 'a vecsxp -> int -> 'b -> unit) (t : 'b array) =
  let s = alloc (Array.length t) in
  Array.iteri (assign s) t ;
  s

let bool_list_of_lglvecsxp x = list_of_vecsxp access_lglvecsxp x
let lglvecsxp_of_bool_list x = vecsxp_of_list alloc_lgl_vector assign_lglvecsxp x
let bool_array_of_lglvecsxp x = array_of_vecsxp access_lglvecsxp x
let lglvecsxp_of_bool_array x = vecsxp_of_array alloc_lgl_vector assign_lglvecsxp x
let bools_of_t tau = bool_array_of_lglvecsxp (cast (tau : lglvecsxp :> sexp) : lglvecsxp)
let bool_of_t tau = access_lglvecsxp (cast (tau : lglvecsxp :> sexp) : lglvecsxp) 0
  (* We access only the first element, because static typing is supposed to
     ensure that the lgl vecsxp contains only one element. *)
let bool b = (cast ((lglvecsxp_of_bool_array [| b |]) : lglvecsxp :> sexp) : lglvecsxp)
let bools bl = (cast ((lglvecsxp_of_bool_array bl) : lglvecsxp :> sexp) : lglvecsxp)

let int_list_of_intvecsxp x  = list_of_vecsxp access_intvecsxp x
let intvecsxp_of_int_list x  = vecsxp_of_list alloc_int_vector assign_intvecsxp x
let int_array_of_intvecsxp x  = array_of_vecsxp access_intvecsxp x
let intvecsxp_of_int_array x  = vecsxp_of_array alloc_int_vector assign_intvecsxp x
let ints_of_t tau = int_array_of_intvecsxp (cast (tau : intvecsxp :> sexp) : intvecsxp)
let int_of_t tau = access_intvecsxp (cast (tau : intvecsxp :> sexp) : intvecsxp) 0
  (* We access only the first element, because static typing is supposed to
     ensure that the int vecsxp contains only one element. *)
let int i = (cast ((intvecsxp_of_int_array [| i |]) : intvecsxp :> sexp) : intvecsxp)
let ints il = (cast ((intvecsxp_of_int_array il) : intvecsxp :> sexp) : intvecsxp)

let float_list_of_realvecsxp x = list_of_vecsxp access_realvecsxp x
let realvecsxp_of_float_list x = vecsxp_of_list alloc_real_vector assign_realvecsxp x
let float_array_of_realvecsxp x = array_of_vecsxp access_realvecsxp x
let realvecsxp_of_float_array x = vecsxp_of_array alloc_real_vector assign_realvecsxp x
let floats_of_t tau = float_array_of_realvecsxp (cast (tau : realvecsxp :> sexp) : realvecsxp)
let float_of_t tau = access_realvecsxp (cast (tau : realvecsxp :> sexp) : realvecsxp) 0
  (* We access only the first element, because static typing is supposed to
     ensure that the real vecsxp contains only one element. *)
let float x = (cast ((realvecsxp_of_float_array [| x |]) : realvecsxp :> sexp) : realvecsxp)
let floats xl = (cast ((realvecsxp_of_float_array xl) : realvecsxp :> sexp) : realvecsxp)

let realvecsxp_of_float_option_list x = vecsxp_of_list alloc_real_vector assign_realvecsxp_opt x
let optfloats xl = (cast ((realvecsxp_of_float_option_list xl) : realvecsxp :> sexp) : realvecsxp)
let realvecsxp_of_float_option_array x = vecsxp_of_array alloc_real_vector assign_realvecsxp_opt x
let optfloats xl = (cast ((realvecsxp_of_float_option_array xl) : realvecsxp :> sexp) : realvecsxp)

let string_list_of_strvecsxp x = list_of_vecsxp access_strvecsxp x
let strvecsxp_of_string_list x = vecsxp_of_list alloc_str_vector assign_strvecsxp x
let string_array_of_strvecsxp x = array_of_vecsxp access_strvecsxp x
let strvecsxp_of_string_array x = vecsxp_of_array alloc_str_vector assign_strvecsxp x
let string_list_of_t tau = string_list_of_strvecsxp (cast (tau : strvecsxp :> sexp) : strvecsxp)
let strings_of_t tau = string_array_of_strvecsxp (cast (tau : strvecsxp :> sexp) : strvecsxp)
let string_of_t tau = access_strvecsxp (cast (tau : strvecsxp :> sexp) : strvecsxp) 0
  (* We access only the first element, because static typing is supposed to
     ensure that the str vecsxp contains only one element. *)
external string : string -> strvecsxp = "ocamlr_strsxp_of_string"
let strings sl = (cast ((strvecsxp_of_string_array sl) : strvecsxp :> sexp) : strvecsxp)

let sexp_list_of_rawvecsxp x = list_of_vecsxp access_rawvecsxp x
let sexps_of_t tau = sexp_list_of_rawvecsxp (cast (tau : rawvecsxp :> sexp) : rawvecsxp)

let langsxp_list_of_exprvecsxp x = list_of_vecsxp access_exprvecsxp x
let langsxps_of_t tau = langsxp_list_of_exprvecsxp (cast (tau : rawvecsxp :> sexp) : exprvecsxp)

(* === INTERNAL ===== *)


let is_nil x = sexptype (x : _ t :> sexp) = NilSxp
let nil_map x ~f =
  if is_nil x then None
  else Some (f x)

module Specification = struct

  type symbol = (string * (sexp option)) option option

  let of_symbol (s : symsxp) =
    let pname    = inspect_symsxp_pname    s
    and value    = inspect_symsxp_value    s
    and internal = inspect_symsxp_internal s in
    match (sexptype pname), (sexptype value), (sexptype internal) with
    | (NilSxp,  _, NilSxp) when sexp_equality (s : symsxp :> sexp) value -> None
    | (CharSxp, SymSxp, NilSxp) ->
        begin match (sexp_equality (s : symsxp :> sexp) value) &&
                    ("" = string_of_charsxp (cast pname : charvecsxp)) with
        | true -> Some None | false ->
        begin match (sexp_equality value (inspect_symsxp_value (cast value : symsxp)))  &&
                    (NilSxp = sexptype (inspect_symsxp_pname (cast value : symsxp)))    &&
                    (NilSxp = sexptype (inspect_symsxp_internal (cast value : symsxp))) with
        | true -> Some (Some ((string_of_charsxp (cast pname : charvecsxp)), None))
        | false -> assert false
        end end
    | (CharSxp, _, NilSxp) ->
        let symbol_name = string_of_charsxp (cast pname : charvecsxp) in
        Some (Some (symbol_name, (Some value)))
    | _ -> assert false

end

let attributes sexp =
  let f (a, x) = (Specification.of_symbol a), x in
  List.map f (list_of_pairlist sexp)

let notnil x =
  if sexptype x = NilSxp then None
  else Some x

module type Types = sig

  type t
  val recursive : t Lazy.t -> t
  val build : (sexp -> t) -> sexp -> t

end

module Parsing (M : Types) = struct

  (* General parsing function for internal R structures, i.e. SEXPs. *)

  let t_of_sexp (s : sexp) =
    let rec aux sexps_seen s =
      let is_found (ss, _) = sexp_equality s ss in
      begin match (try Some (List.find is_found sexps_seen) with _ -> None) with
      | None -> let rec x = lazy (M.build (aux ((s, x)::sexps_seen)) s) in Lazy.force x
      | Some (_, t_lazy) -> M.recursive t_lazy
      end
    in aux [] s

end

module CTypes = struct

  (* Type definitions. *)

  type t = | Recursive of t Lazy.t | Val of t_val

  and t_val = {
    (* sxpinfo : sxpinfo;   *)
    (* attrib  : t;         *)
    (* gengc_nextnode : t;  *)
    (* gengc_prevnode : t;  *)
    content : t_content
  }

  (* and sxpinfo = {
    type  : sexptype;
    obj   : int;
    named : int;
    gp    : int;
    mark  : int;
    debug : int;
    trace : int;
    spare : int;
    gcgen : int;
    gccls : int;
  }*)

  and t_content =
    | NILSXP
    | SYMSXP of sxp_sym
    | LISTSXP of sxp_list
    | CLOSXP of sxp_clos
    | ENVSXP of sxp_env
    | PROMSXP of sxp_prom
    | LANGSXP of sxp_list
    | SPECIALSXP
    | BUILTINSXP of int
    | CHARSXP of string
    | LGLSXP of bool list
    | INTSXP of int list
    | REALSXP of float list
    | CPLXSXP
    | STRSXP of string list
    | DOTSXP
    | ANYSXP
    | VECSXP of t list
    | EXPRSXP
    | BCODESXP
    | EXTPTRSXP
    | WEAKREFSXP
    | RAWSXP
    | S4SXP
    | FUNSXP

  and sxp_sym  = { pname: t; sym_value: t; internal: t }
  and sxp_list = { carval: t; cdrval: t; tagval: t }
  and sxp_env  = { frame: t; (*enclos: t; hashtab: t*) }
  and sxp_clos = { formals: t; body: t; clos_env: t }
  and sxp_prom = { prom_value: t; expr: t; prom_env: t }

  let recursive x = Recursive (lazy (Lazy.force x))

  let build rec_build s =
    match sexptype s with
    | NilSxp     -> Val { content = NILSXP }
    | SymSxp     -> Val { content = SYMSXP {
        pname      = rec_build (inspect_symsxp_pname    (cast s : symsxp));
        sym_value  = rec_build (inspect_symsxp_value    (cast s : symsxp));
        internal   = rec_build (inspect_symsxp_internal (cast s : symsxp))}}
    | ListSxp    -> Val { content = LISTSXP {
        carval     = rec_build (inspect_listsxp_carval  (cast s : pairlistsxp));
        cdrval     = rec_build (inspect_listsxp_cdrval  (cast s : pairlistsxp));
        tagval     = rec_build (inspect_listsxp_tagval  (cast s : pairlistsxp))}}
    | CloSxp     -> Val { content = CLOSXP {
        formals    = rec_build (inspect_closxp_formals  (cast s : closxp));
        body       = rec_build (inspect_closxp_body     (cast s : closxp));
        clos_env   = rec_build (inspect_closxp_env      (cast s : closxp))}}
    | EnvSxp     -> Val { content = ENVSXP {
        frame      = rec_build (inspect_envsxp_frame    (cast s : envsxp));
     (* enclos     = rec_build (inspect_envsxp_enclos   s); *)
     (* hashtab    = rec_build (inspect_envsxp_hashtab  s) *) }}
    | PromSxp    -> Val { content = PROMSXP {
        prom_value = rec_build (inspect_promsxp_value  (cast s : promsxp));
        expr       = rec_build (inspect_promsxp_expr   (cast s : promsxp));
        prom_env   = rec_build (inspect_promsxp_env    (cast s : promsxp))}}
    | LangSxp    -> Val { content = LANGSXP {
        carval     = rec_build (inspect_listsxp_carval (cast s : langsxp));
        cdrval     = rec_build (inspect_listsxp_cdrval (cast s : langsxp));
        tagval     = rec_build (inspect_listsxp_tagval (cast s : langsxp))}}
    | SpecialSxp -> Val { content = SPECIALSXP }
    | BuiltinSxp -> Val { content = BUILTINSXP (inspect_primsxp_offset (cast s : builtinsxp))}
    | CharSxp    -> Val { content = CHARSXP (string_of_charsxp (cast s : charvecsxp)) }
    | LglSxp     -> Val { content = LGLSXP (bool_list_of_lglvecsxp (cast s : lglvecsxp))}
    | IntSxp     -> Val { content = INTSXP (int_list_of_intvecsxp (cast s : intvecsxp))}
    | RealSxp    -> Val { content = REALSXP (float_list_of_realvecsxp (cast s : realvecsxp))}
    | CplxSxp    -> Val { content = CPLXSXP }
    | StrSxp     -> Val { content = STRSXP (string_list_of_strvecsxp (cast s: strvecsxp))}
    | DotSxp     -> Val { content = DOTSXP }
    | AnySxp     -> Val { content = ANYSXP }
    | VecSxp     -> Val { content = VECSXP (List.map rec_build (sexp_list_of_rawvecsxp (cast s : rawvecsxp)))}
    | ExprSxp    -> Val { content = EXPRSXP }
    | BcodeSxp   -> Val { content = BCODESXP }
    | ExtptrSxp  -> Val { content = EXTPTRSXP }
    | WeakrefSxp -> Val { content = WEAKREFSXP }
    | RawSxp     -> Val { content = RAWSXP }
    | S4Sxp      -> Val { content = S4SXP }
    | FunSxp     -> Val { content = FUNSXP }

end

module PrettyTypes = struct

  type t =
    | Recursive of t Lazy.t
    | NULL
    | SYMBOL of (string * t) option
    | ARG of string
    | PLACE
    | LIST of pairlist
    | CLOSURE of closure
    | ENV of environment
    | PROMISE of promise
    | CALL of t * pairlist
    | SPECIAL of int
    | BUILTIN
    | STRING of string
    | STRINGS of string list
    | INTS of int list
    | VECSXP of t list
    | BOOLS of bool list
    | FLOATS of float list
    | Unknown

  and closure     = { formals: t; body: t; clos_env: t }
  and environment = { frame: t; (* enclos: t; hashtab: t *) }
  and promise     = { value: t; expr: t; prom_env: t }

  and pairlist = (t * t) list (* For strict list parsing, t list. *)

  let recursive x = Recursive (lazy (Lazy.force x))

  exception Sexp_to_inspect of sexp
  exception Esoteric of sexp

  let symbol_of_symsxp builder (s : symsxp) =
    match begin try Some (Specification.of_symbol s) with
                | Assert_failure _ -> None end with
    | None -> raise (Esoteric (s : symsxp :> sexp))
    | Some None -> SYMBOL None
    | Some (Some None) -> PLACE
    | Some (Some (Some (symbol_name, None))) -> ARG symbol_name
    | Some (Some (Some (symbol_name, Some v))) -> SYMBOL (Some (symbol_name, (builder v)))

  let rec list_of_listsxp builder (s : 'a listsxp) =
    let carval = inspect_listsxp_carval s
    and cdrval = inspect_listsxp_cdrval s
    and tagval = inspect_listsxp_tagval s in
    (* Strict parsing of the LIST:
    LIST begin match sexptype tagval with
    | NilSxp ->  (builder carval) :: begin
                 match builder cdrval with
                 | LIST l -> l | NULL -> []
                 | _ -> raise (Esoteric s) end
    | _ -> raise Esoteric end *)
    (* Lax parsing of the LIST: *)
    LIST begin ((builder tagval), (builder carval))::
      begin match builder cdrval with
      | LIST l -> l | NULL -> []
      | _ -> raise (Esoteric (s : 'a listsxp :> sexp)) end
    end

  let rec build rec_build =
    let phi = fun f -> f (build rec_build) in
    function s -> match sexptype s with
    | NilSxp     -> NULL
    | SymSxp     -> begin try phi symbol_of_symsxp (Obj.magic s) with
                    | Esoteric _ -> Unknown end
    | ListSxp    -> begin try phi list_of_listsxp (cast s : 'a listsxp) with
                    | Esoteric _ -> Unknown end
    | CloSxp     -> CLOSURE {
        formals  = rec_build (inspect_closxp_formals (cast s : closxp));
        body     = rec_build (inspect_closxp_body    (cast s : closxp));
        clos_env = rec_build (inspect_closxp_env     (cast s : closxp))}
    | EnvSxp     -> ENV {
        frame    = rec_build (inspect_envsxp_frame   (cast s : envsxp));
     (* enclos  = rec_build (inspect_envsxp_enclos  s); *) (* We do not care for now. *)
     (* hashtab = rec_build (inspect_envsxp_hashtab s)  *) }
    | PromSxp    -> PROMISE {
        value    = rec_build (inspect_promsxp_value  (cast s : promsxp));
        expr     = rec_build (inspect_promsxp_expr   (cast s : promsxp));
        prom_env = rec_build (inspect_promsxp_env    (cast s : promsxp))}
    | LangSxp    ->
        let carval = inspect_listsxp_carval (cast s : langsxp)
        and cdrval = inspect_listsxp_cdrval (cast s : langsxp)
        and tagval = inspect_listsxp_tagval (cast s : langsxp) in
        begin match build rec_build cdrval with
        | LIST l -> begin match sexptype tagval with
                    | NilSxp -> CALL ((build rec_build carval), l)
                    | _ -> Unknown end
        | _ -> Unknown end
    | SpecialSxp -> SPECIAL (inspect_primsxp_offset (cast s : specialsxp))
    | BuiltinSxp -> BUILTIN
    | CharSxp    -> STRING  (string_of_charsxp (cast s : charvecsxp))
    | LglSxp     -> BOOLS   (bool_list_of_lglvecsxp (cast s : lglvecsxp))
    | IntSxp     -> INTS    (int_list_of_intvecsxp (cast s : intvecsxp))
    | RealSxp    -> FLOATS  (float_list_of_realvecsxp (cast s : realvecsxp))
    | CplxSxp    -> Unknown
    | StrSxp     -> STRINGS (string_list_of_strvecsxp (cast s : strvecsxp))
    | DotSxp     -> Unknown
    | AnySxp     -> Unknown
    | VecSxp     -> VECSXP  (List.map rec_build (sexp_list_of_rawvecsxp (cast s : rawvecsxp)))
    | ExprSxp    -> Unknown
    | BcodeSxp   -> Unknown
    | ExtptrSxp  -> Unknown
    | WeakrefSxp -> Unknown
    | RawSxp     -> Unknown
    | S4Sxp      -> Unknown
    | FunSxp     -> Unknown

end

module CParsed = Parsing (CTypes)

module PrettyParsed = Parsing (PrettyTypes)

module C = struct
  include CTypes
  include CParsed
end

module Pretty = struct
  include PrettyTypes
  include PrettyParsed
end


(* === S3 ===== *)
(**  Get the S3 class of a given SEXP.
  *
  *  s3_class takes a SEXP as argument, and returns the S3 class
  *  attribute of the given SEXP.
  *)
external s3_class : sexp -> sexp = "ocamlr_s3_class"

external aux_get_attrib : sexp -> symsxp -> sexp = "ocamlr_get_attrib"
let get_attrib s name = aux_get_attrib s (install name)

external get_attributes : sexp -> pairlist = "ocamlr_get_attributes"

(* class s3 r = object *)
(*   val __underlying = (r : 'a t :> sexp) *)
(*   method private attribute : 'a. string -> 'a t = function s -> cast (get_attrib __underlying s) *)
(*   method attributes = List.map *)
(*     begin function a, x -> (Specification.of_symbol a), x end *)
(*     (list_of_pairlist (get_attributes __underlying)) *)
(*   method classes = strings_of_t (cast (get_attrib __underlying "class") : strvecsxp) *)
(* end *)

(* let s3 (r : 'a t) = new s3 r *)


let classes sexp =
  string_list_of_t (cast (get_attrib sexp "class") : string list t)

(* === S4 ===== *)


external is_s4_object : sexp -> bool = "ocamlr_is_s4_object"

external do_new_object : sexp -> sexp = "ocamlr_do_new_object"


(* === R_PARSER ===== *)


type parse_status =
  | Parse_Null
  | Parse_OK
  | Parse_Incomplete
  | Parse_Error
  | Parse_EOF

exception Parsing_failure of parse_status * string

let parse_status_of_int = function
  | 0 -> Parse_Null
  | 1 -> Parse_OK
  | 2 -> Parse_Incomplete
  | 3 -> Parse_Error
  | 4 -> Parse_EOF
  | _ -> assert false

external raw_parse_string : string -> int -> int * sexp = "ocamlr_parse_string"
let parse_string ?max statement =
  let error_code, sexp = raw_parse_string statement
    begin match max with None -> -1 | Some n -> n end in
  match parse_status_of_int error_code with
  | Parse_OK -> langsxps_of_t (cast sexp : rawvecsxp)
  | _ as status -> raise (Parsing_failure (status, statement))

let parse statement = List.hd (parse_string ~max:1 statement)


(* === REDUCTION ===== *)


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

(* === INITIALISATION ===== *)


external initialise : string array -> int -> int = "ocamlr_initEmbeddedR" [@@noalloc]
external terminate : unit -> unit = "ocamlr_endEmbeddedR" [@@noalloc]

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
  | 1 ->
      Callback.register_exception
        "OCaml-R generic error"
           (* The Runtime_error is initialised with a nilsxp casted to a langsxp.
              This is ugly, but not unsafe. *)
        (Runtime_error ((cast ((null_creator ()) : nilsxp :> sexp ) : langsxp ), ""))
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
