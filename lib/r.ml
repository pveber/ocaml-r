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

(* === SEXPTYPE ===== *)


(* The type system of OCaml-R. *)

type sexp

module type SXP = sig
  type t

  val equal : t -> t -> bool
  val is_function : t -> bool
  val attr : t -> string -> sexp
  val _class_ : t -> string list
  val nil_map : t -> f:(t -> 'a) -> 'a option
  external unsafe_of_sexp : sexp -> t = "%identity"
  external to_sexp : t -> sexp = "%identity"
end


module Sxp : sig
  type +'a t = private sexp

  module type Constraint = sig
    type t = private [> ]
  end

  module Impl(K : Constraint)(I : SXP with type t := sexp) : SXP with type t = K.t t
end
=
struct
  type +'a t = sexp

  module type Constraint = sig
    type t = private [> ]
  end

  module Impl(K : Constraint)(I : SXP with type t := sexp) = struct
    type nonrec t = K.t t
    include I
  end
end

type 'a sxp = 'a Sxp.t

type nilsxp      = [`Nil]     sxp
type symsxp      = [`Sym]     sxp
type langsxp     = [`Lang]    sxp
type listsxp     = [`List]    sxp
type dotsxp      = [`Dot]     sxp
type closxp      = [`Clo]     sxp
type envsxp      = [`Env]     sxp
type promsxp     = [`Prom]    sxp
type specialsxp  = [`Special] sxp
type builtinsxp  = [`Builtin] sxp
type vecsxp      = [`Vec]     sxp
type charsxp     = [`Char]    sxp
type lglsxp      = [`Lgl]     sxp
type intsxp      = [`Int]     sxp
type realsxp     = [`Real]    sxp
type strsxp      = [`Str]     sxp
type rawsxp      = [`Raw]     sxp
type exprsxp     = [`Expr]    sxp

type 'a nonempty_list = [< `List | `Lang | `Dots] as 'a
(** R-ints: Language objects (LANGSXP) are calls (including formulae
   and so on). Internally they are pairlists with first element a
   reference to the function to be called with remaining elements the
   actual arguments for the call. Although this is not enforced, many
   places in the code assume that the pairlist is of length one or
   more, often without checking. *)

type internallist = [ `Nil | `List | `Lang | `Dots]
(**  Type of low-level internal list. In R, such
  *  internal lists may be empty, a pairlist or
  *  a call which is somewhat similar to closure
  *  ready for execution. *)

type 'a at_most_internallist = [< internallist] as 'a

type 'a pairlist = [< `Nil | `List] as 'a

type vector = [ `Char | `Lgl | `Int  | `Real
              | `Str  | `Raw | `Expr | `Vec]
type 'a at_most_vector = [< vector] as 'a

external upcast : sexp -> 'a sxp = "%identity"


module Low_level = struct
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


  (* === READ_INTERNAL ===== *)

  (* Low-level data manipulation functions. *)

  (* What follows is low-level accessor functions, in order to inspect
     in details the contents of SEXPs and VECSEXPs. *)

  external inspect_attributes : sexp -> sexp = "ocamlr_inspect_attributes"
  external length_of_vector   : 'a at_most_vector sxp -> int  = "ocamlr_inspect_vecsxp_length"

  external inspect_primsxp_offset  : [< `Special | `Builtin ] sxp -> int = "ocamlr_inspect_primsxp_offset"
  external inspect_symsxp_pname    : symsxp         -> sexp          = "ocamlr_inspect_symsxp_pname"
  external inspect_symsxp_value    : symsxp         -> sexp          = "ocamlr_inspect_symsxp_value"
  external inspect_symsxp_internal : symsxp         -> sexp          = "ocamlr_inspect_symsxp_internal"
  external inspect_listsxp_carval  : 'a nonempty_list sxp -> sexp    = "ocamlr_inspect_listsxp_carval"
  external inspect_listsxp_cdrval  : 'a nonempty_list sxp -> [> internallist] sxp = "ocamlr_inspect_listsxp_cdrval"
  external inspect_listsxp_tagval  : 'a nonempty_list sxp -> sexp    = "ocamlr_inspect_listsxp_tagval"
  external inspect_envsxp_frame    : envsxp         -> sexp          = "ocamlr_inspect_envsxp_frame"
  external inspect_envsxp_enclos   : envsxp         -> sexp          = "ocamlr_inspect_envsxp_enclos"
  external inspect_envsxp_hashtab  : envsxp         -> sexp          = "ocamlr_inspect_envsxp_hashtab"
  external inspect_closxp_formals  : closxp         -> sexp          = "ocamlr_inspect_closxp_formals"
  external inspect_closxp_body     : closxp         -> sexp          = "ocamlr_inspect_closxp_body"
  external inspect_closxp_env      : closxp         -> sexp          = "ocamlr_inspect_closxp_env"
  external inspect_promsxp_value   : promsxp        -> sexp          = "ocamlr_inspect_promsxp_value"
  external inspect_promsxp_expr    : promsxp        -> sexp          = "ocamlr_inspect_promsxp_expr"
  external inspect_promsxp_env     : promsxp        -> sexp          = "ocamlr_inspect_promsxp_env"

  external access_lglsxp  : lglsxp  -> int -> bool     = "ocamlr_access_lglsxp"
  external access_lglsxp_opt : lglsxp  -> int -> bool option = "ocamlr_access_lglsxp_opt"
  external access_intsxp  : intsxp  -> int -> int      = "ocamlr_access_intsxp"
  external access_intsxp_opt  : intsxp  -> int -> int option = "ocamlr_access_intsxp_opt"
  external access_realsxp : realsxp -> int -> float    = "ocamlr_access_realsxp"
  external access_realsxp_opt : realsxp -> int -> float option = "ocamlr_access_realsxp_opt"
  external access_strsxp  : strsxp  -> int -> string   = "ocamlr_access_strsxp"
  external access_strsxp_opt  : strsxp  -> int -> string option = "ocamlr_access_strsxp_opt"
  external access_vecsxp  : vecsxp  -> int -> sexp     = "ocamlr_access_vecsxp"
  external access_rawsxp  : rawsxp  -> int -> sexp     = "ocamlr_access_vecsxp"
  external access_exprsxp : exprsxp -> int -> langsxp  = "ocamlr_access_vecsxp"

  (* === ALLOCATION ===== *)

  (* This file contains wrappers around allocation functions,
     returning uninitialised pairlists and vectors. *)

  external alloc_list        : int -> [> internallist] sxp = "ocamlr_alloc_list"
  external alloc_lglsxp      : int -> lglsxp               = "ocamlr_alloc_lglsxp"
  external alloc_intsxp      : int -> intsxp               = "ocamlr_alloc_intsxp"
  external alloc_real_vector : int -> realsxp              = "ocamlr_alloc_realsxp"
  external alloc_str_vector  : int -> strsxp               = "ocamlr_alloc_strsxp"
  external alloc_vecsxp      : int -> vecsxp               = "ocamlr_alloc_vecsxp"

  (* === WRITE_INTERNAL ===== *)


  external write_listsxp_carval : 'a nonempty_list sxp -> sexp -> unit = "ocamlr_write_lisplist_carval"
  external write_listsxp_tagval : 'a nonempty_list sxp -> sexp -> unit = "ocamlr_write_lisplist_tagval"

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

  external assign_lglsxp     : lglsxp -> int -> bool -> unit = "ocamlr_assign_lglsxp"
  external assign_lglsxp_opt : lglsxp -> int -> bool option -> unit = "ocamlr_assign_lglsxp_opt"


  (**  Sets the element of a vector of integers.
    *
    *  assign_int_vecsxp takes a vector of integers as first argument,
    *  an offset as second argument, and an integer as third argument,
    *  and sets the vector's offset element to the integer's value.
    *
    *  Question: should we rather map R's integers to int32s?
  *)

  external assign_intsxp  : intsxp -> int -> int -> unit = "ocamlr_assign_intsxp"


  (**  Sets the element of a vector of integers.
    *
    *  assign_int_vecsxp takes a vector of integers as first argument,
    *  an offset as second argument, and an integer as third argument,
    *  and sets the vector's offset element to the integer's value.
    *
    *  Question: should we rather map R's integers to int32s?
  *)

  external assign_intsxp_opt  : intsxp -> int -> int option -> unit = "ocamlr_assign_intsxp_opt"


  (**  Sets the element of a vector of real numbers.
    *
    *  assign_real_vecsxp takes a vector of real numbers as first argument,
    *  an offset as second argument, and a real number as third argument,
    *  and sets the vector's offset element to the real number's value.
  *)

  external assign_realsxp : realsxp -> int -> float -> unit = "ocamlr_assign_realsxp"


  (**  Sets the element of a vector of real numbers with possibly missing values
    *
    *  assign_real_vecsxp takes a vector of real numbers as first argument,
    *  an offset as second argument, and a possibly missig real number as third argument,
    *  and sets the vector's offset element to the real number's value or NA if non available.
  *)

  external assign_realsxp_opt : realsxp -> int -> float option -> unit = "ocamlr_assign_realsxp_opt"


  (**  Sets the element of a vector of string.
    *
    *  assign_str_vecsxp takes a vector of strings as first argument,
    *  an offset as second argument, and a string as third argument,
    *  and sets the vector's offset element to the string's value.
  *)

  external assign_strsxp  : strsxp -> int -> string -> unit = "ocamlr_assign_strsxp"
  external assign_strsxp_opt : strsxp -> int -> string option -> unit = "ocamlr_assign_strsxp_opt"

  external assign_vecsxp  : vecsxp -> int -> sexp -> unit = "ocamlr_assign_vecsxp"

  (* === SEXPREC ===== *)

  external sexp_equality : sexp -> sexp -> bool = "ocamlr_sexp_equality"

  (* R constants - global symbols in libR.so. *)
  (* We are looking for a clean solution
     for the typing of the R NULL. What should it be
     in OCaml? An 'a option mapping to None? *)
  external null_creator : unit -> [> `Nil] sxp = "ocamlr_null"
  external dots_symbol_creator : unit -> [> `Dot] sxp = "ocamlr_dots_symbol"
  external missing_arg_creator : unit -> sexp = "ocamlr_missing_arg"
  external base_env_creator : unit -> sexp = "ocamlr_base_env"

  (* R_GlobalEnv is not a constant, but rather a constant pointer,
     that gets updated by R itself. *)
  external global_env : unit -> sexp = "ocamlr_global_env"

  external cons : sexp -> [< `Nil | `List] sxp -> [> `List] sxp = "ocamlr_cons"
  external tag : listsxp -> string -> unit = "ocamlr_tag"

  external set_langsxp : listsxp -> unit = "ocamlr_set_langsxp"
  (* Beware: [set_langsxp x] breaks typing, since after the call, [x] is
     now of type [langsxp]. *)

  (* === S3 ===== *)
  (**  Get the S3 class of a given SEXP.
    *
    *  s3_class takes a SEXP as argument, and returns the S3 class
    *  attribute of the given SEXP.
  *)
  external s3_class : sexp -> strsxp = "ocamlr_s3_class"

  external aux_get_attrib : sexp -> symsxp -> sexp = "ocamlr_get_attrib"
  let get_attrib s name = aux_get_attrib s (install name)

  external get_attributes : sexp -> _ pairlist sxp = "ocamlr_get_attributes"

  (* === S4 ===== *)

  external is_s4_object : sexp -> bool = "ocamlr_is_s4_object"

  external do_new_object : sexp -> sexp = "ocamlr_do_new_object"

  (* ==== REDUCTION ==== *)
  external eval_langsxp : langsxp -> sexp = "ocamlr_eval_sxp"

  external string_of_charsxp : charsxp -> string = "ocamlr_internal_string_of_charsxp"

  let list_of_vector (access : 'a at_most_vector sxp -> int -> 'b) (s : 'a at_most_vector sxp) =
    let lngth = length_of_vector s in
    let rec aux n accu = match n with | 0 -> accu | _ ->
      let x = access s (n - 1) in aux (n - 1) (x :: accu)
    in aux lngth []

  let vector_of_list (alloc : int -> 'a at_most_vector sxp) (assign : 'a at_most_vector sxp -> int -> 'b -> unit) (l: 'b list) =
    let s = alloc (List.length l) in
    let rec aux offset = function | [] -> () | hd::tl ->
      let () = assign s offset hd in aux (1 + offset) tl
    in aux 0 l; s

  let array_of_vector (access : 'a at_most_vector sxp -> int -> 'b) (s : 'a at_most_vector sxp) =
    let lngth = length_of_vector s in
    Array.init lngth (access s)

  let vector_of_array (alloc : int -> 'a at_most_vector sxp) (assign : 'a at_most_vector sxp -> int -> 'b -> unit) (t : 'b array) =
    let s = alloc (Array.length t) in
    Array.iteri (assign s) t ;
    s

  let bool_list_of_lglsxp x = list_of_vector access_lglsxp x
  let lglsxp_of_bool_list x = vector_of_list alloc_lglsxp assign_lglsxp x

  let int_list_of_intsxp x  = list_of_vector access_intsxp x
  let intsxp_of_int_list x  = vector_of_list alloc_intsxp assign_intsxp x

  let float_list_of_realsxp x = list_of_vector access_realsxp x
  let realsxp_of_float_list x = vector_of_list alloc_real_vector assign_realsxp x

  let realsxp_of_float_option_list x = vector_of_list alloc_real_vector assign_realsxp_opt x

  let string_list_of_strsxp x = list_of_vector access_strsxp x
  let strsxp_of_string_list x = vector_of_list alloc_str_vector assign_strsxp x
  let string_list_of_t tau = string_list_of_strsxp tau

  let sexp_list_of_rawsxp x = list_of_vector access_rawsxp x
  let sexps_of_t tau = sexp_list_of_rawsxp tau

  let langsxp_list_of_exprsxp x = list_of_vector access_exprsxp x
  let langsxps_of_t tau = langsxp_list_of_exprsxp tau

  let classes sexp =
    string_list_of_t (upcast (get_attrib sexp "class") : strsxp)

end

open Low_level

external cast : _ sxp -> _ sxp = "%identity"


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

module Sexptype = struct
  type t = sexptype =
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

  let of_sexp = sexptype

  let to_string = function
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
end

let is_function x = match sexptype x with
  | CloSxp | SpecialSxp | BuiltinSxp | FunSxp -> true
  | _ -> false



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



(* === CONVERSION  ===== *)

let rec list_of_pairlist (ll : [< internallist] sxp) =
  match sexptype (ll : 'a at_most_internallist sxp :> sexp) with
  | NilSxp -> []
  | ListSxp | LangSxp | DotSxp ->
    let ll : _ nonempty_list sxp = upcast (ll : 'a at_most_internallist sxp :> sexp) in
    (
      (upcast (inspect_listsxp_tagval ll) : symsxp (* TODO: This may be excessive *)),
      inspect_listsxp_carval ll
    )
    :: list_of_pairlist (inspect_listsxp_cdrval ll)
  | _ -> failwith "Conversion failure in list_of_listsxp."

let pairlist_of_list (l: (sexp * sexp) list) =
  let r_l = alloc_list (List.length l) in
  let cursor = ref r_l in
  List.iter (function (tag, value) ->
      write_listsxp_element (cast !cursor) tag value ;
      cursor := inspect_listsxp_cdrval (cast !cursor)
      (* [cursor] is not typeable because it is a non empty list until
         we reach the end. But those casts are not unsafe since we
         don't call [write_listsxp_element] when [cursor] becomes
         nil. *)
    ) l ;
  r_l

let langsxp (f: sexp) (args: (string option * sexp) list) : langsxp =
  let lcons hd tl =
    let x = cons hd tl in
    set_langsxp x ; (* here, type of [x] has been changed to [langsxp]! *)
    (upcast (x : listsxp :> sexp) : langsxp)
  in
  let g (t, hd) tl =
    let x = cons hd tl in match t with
    | None -> x
    | Some name -> tag x name ; x
  in
  let args_as_listsxp =
    List.fold_right g args (null_creator ())
  in
  lcons f args_as_listsxp


(* === INTERNAL ===== *)


let is_nil x = sexptype (x :> sexp) = NilSxp

module Specification = struct

  type symbol = (string * (sexp option)) option option

  let of_symbol (s : symsxp) =
    let pname    = inspect_symsxp_pname    s
    and value    = inspect_symsxp_value    s
    and internal = inspect_symsxp_internal s in
    match sexptype pname, sexptype value, sexptype internal with
    | (NilSxp,  _, NilSxp) when sexp_equality (s : symsxp :> sexp) value -> None
    | (CharSxp, SymSxp, NilSxp) -> (
        match (sexp_equality (s : symsxp :> sexp) value) &&
              ("" = string_of_charsxp (upcast pname : charsxp)) with
        | true -> Some None
        | false -> (
            match (sexp_equality value (inspect_symsxp_value (upcast value : symsxp)))  &&
                  (NilSxp = sexptype (inspect_symsxp_pname (upcast value : symsxp)))    &&
                  (NilSxp = sexptype (inspect_symsxp_internal (upcast value : symsxp))) with
            | true -> Some (Some ((string_of_charsxp (upcast pname : charsxp)), None))
            | false -> assert false
          )
      )
    | (CharSxp, _, (NilSxp | BuiltinSxp)) ->
      let symbol_name = string_of_charsxp (upcast pname : charsxp) in
      Some (Some (symbol_name, (Some value)))
    | _ -> assert false

end

let attributes sexp =
  let f (a, x) = (Specification.of_symbol a), x in
  List.map f (list_of_pairlist (upcast sexp))

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
        pname      = rec_build (inspect_symsxp_pname    (upcast s : symsxp));
        sym_value  = rec_build (inspect_symsxp_value    (upcast s : symsxp));
        internal   = rec_build (inspect_symsxp_internal (upcast s : symsxp))}}
    | ListSxp    ->
      let s = (upcast s : listsxp) in
      Val { content = LISTSXP {
        carval     = rec_build (inspect_listsxp_carval  s);
        cdrval     = rec_build ((inspect_listsxp_cdrval s) : internallist sxp :> sexp) ;
        tagval     = rec_build (inspect_listsxp_tagval  s)}}
    | CloSxp     -> Val { content = CLOSXP {
        formals    = rec_build (inspect_closxp_formals  (upcast s : closxp));
        body       = rec_build (inspect_closxp_body     (upcast s : closxp));
        clos_env   = rec_build (inspect_closxp_env      (upcast s : closxp))}}
    | EnvSxp     -> Val { content = ENVSXP {
        frame      = rec_build (inspect_envsxp_frame    (upcast s : envsxp));
     (* enclos     = rec_build (inspect_envsxp_enclos   s); *)
     (* hashtab    = rec_build (inspect_envsxp_hashtab  s) *) }}
    | PromSxp    -> Val { content = PROMSXP {
        prom_value = rec_build (inspect_promsxp_value  (upcast s : promsxp));
        expr       = rec_build (inspect_promsxp_expr   (upcast s : promsxp));
        prom_env   = rec_build (inspect_promsxp_env    (upcast s : promsxp))}}
    | LangSxp    ->
      let s = (upcast s : langsxp) in
      Val { content = LANGSXP {
        carval     = rec_build (inspect_listsxp_carval s);
        cdrval     = rec_build (inspect_listsxp_cdrval s : internallist sxp :> sexp);
        tagval     = rec_build (inspect_listsxp_tagval s)}}
    | SpecialSxp -> Val { content = SPECIALSXP }
    | BuiltinSxp -> Val { content = BUILTINSXP (inspect_primsxp_offset (upcast s : builtinsxp))}
    | CharSxp    -> Val { content = CHARSXP (string_of_charsxp (upcast s : charsxp)) }
    | LglSxp     -> Val { content = LGLSXP (bool_list_of_lglsxp (upcast s : lglsxp))}
    | IntSxp     -> Val { content = INTSXP (int_list_of_intsxp (upcast s : intsxp))}
    | RealSxp    -> Val { content = REALSXP (float_list_of_realsxp (upcast s : realsxp))}
    | CplxSxp    -> Val { content = CPLXSXP }
    | StrSxp     -> Val { content = STRSXP (string_list_of_strsxp (upcast s: strsxp))}
    | DotSxp     -> Val { content = DOTSXP }
    | AnySxp     -> Val { content = ANYSXP }
    | VecSxp     -> Val { content = VECSXP (List.map rec_build (sexp_list_of_rawsxp (upcast s : rawsxp)))}
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

  exception Esoteric of sexp

  let symbol_of_symsxp builder (s : symsxp) =
    match Specification.of_symbol s with
    | exception Assert_failure _ -> raise (Esoteric (s : symsxp :> sexp))
    | None -> SYMBOL None
    | Some None -> PLACE
    | Some (Some (symbol_name, None)) -> ARG symbol_name
    | Some (Some (symbol_name, Some v)) -> SYMBOL (Some (symbol_name, (builder v)))

  let list_of_listsxp builder (s : 'a nonempty_list sxp) =
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
      begin match builder (cdrval : _ sxp :> sexp) with
      | LIST l -> l | NULL -> []
      | _ -> raise (Esoteric (s : _ sxp :> sexp)) end
    end

  let rec build rec_build =
    let phi = fun f -> f (build rec_build) in
    function s -> match sexptype s with
    | NilSxp     -> NULL
    | SymSxp     -> begin try phi symbol_of_symsxp (Obj.magic s) with
                    | Esoteric _ -> Unknown end
    | ListSxp    -> begin try phi list_of_listsxp (upcast s : 'a nonempty_list sxp) with
                    | Esoteric _ -> Unknown end
    | CloSxp     -> CLOSURE {
        formals  = rec_build (inspect_closxp_formals (upcast s : closxp));
        body     = rec_build (inspect_closxp_body    (upcast s : closxp));
        clos_env = rec_build (inspect_closxp_env     (upcast s : closxp))}
    | EnvSxp     -> ENV {
        frame    = rec_build (inspect_envsxp_frame   (upcast s : envsxp));
     (* enclos  = rec_build (inspect_envsxp_enclos  s); *) (* We do not care for now. *)
     (* hashtab = rec_build (inspect_envsxp_hashtab s)  *) }
    | PromSxp    -> PROMISE {
        value    = rec_build (inspect_promsxp_value  (upcast s : promsxp));
        expr     = rec_build (inspect_promsxp_expr   (upcast s : promsxp));
        prom_env = rec_build (inspect_promsxp_env    (upcast s : promsxp))}
    | LangSxp    ->
        let carval = inspect_listsxp_carval (upcast s : langsxp)
        and cdrval = inspect_listsxp_cdrval (upcast s : langsxp)
        and tagval = inspect_listsxp_tagval (upcast s : langsxp) in
        begin match build rec_build (cdrval : _ sxp :> sexp) with
        | LIST l -> begin match sexptype tagval with
                    | NilSxp -> CALL ((build rec_build carval), l)
                    | _ -> Unknown end
        | _ -> Unknown end
    | SpecialSxp -> SPECIAL (inspect_primsxp_offset (upcast s : specialsxp))
    | BuiltinSxp -> BUILTIN
    | CharSxp    -> STRING  (string_of_charsxp (upcast s : charsxp))
    | LglSxp     -> BOOLS   (bool_list_of_lglsxp (upcast s : lglsxp))
    | IntSxp     -> INTS    (int_list_of_intsxp (upcast s : intsxp))
    | RealSxp    -> FLOATS  (float_list_of_realsxp (upcast s : realsxp))
    | CplxSxp    -> Unknown
    | StrSxp     -> STRINGS (string_list_of_strsxp (upcast s : strsxp))
    | DotSxp     -> Unknown
    | AnySxp     -> Unknown
    | VecSxp     -> VECSXP  (List.map rec_build (sexp_list_of_rawsxp (upcast s : rawsxp)))
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



(* class s3 r = object *)
(*   val __underlying = (r : 'a t :> sexp) *)
(*   method private attribute : 'a. string -> 'a t = function s -> upcast (get_attrib __underlying s) *)
(*   method attributes = List.map *)
(*     begin function a, x -> (Specification.of_symbol a), x end *)
(*     (list_of_pairlist (get_attributes __underlying)) *)
(*   method classes = strings_of_t (upcast (get_attrib __underlying "class") : strsxp) *)
(* end *)

(* let s3 (r : 'a t) = new s3 r *)





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
  | Parse_OK -> langsxps_of_t (upcast sexp : exprsxp)
  | _ as status -> raise (Parsing_failure (status, statement))

let parse statement = List.hd (parse_string ~max:1 statement)

module Sexp = struct
  type t = sexp
  let is_function x =
    match sexptype x with
    | CloSxp | SpecialSxp | BuiltinSxp | FunSxp -> true
    | _ -> false

  let equal x y = sexp_equality x y

  let _class_ x =
    s3_class x
    |> list_of_vector access_strsxp

  let attr x s = get_attrib x s

  external unsafe_of_sexp : sexp -> sexp = "%identity"
  external to_sexp : sexp -> sexp = "%identity"

  let nil_map x ~f =
    if is_nil x then None
    else Some (f x)
end

module Nilsxp = struct
  include Sxp.Impl(struct type t = [`Nil] end)(Sexp)
  let create = null_creator
end

module Dotsxp = struct
  include Sxp.Impl(struct type t = [`Dot] end)(Sexp)
  let create = dots_symbol_creator
end

module Envsxp = struct
  include Sxp.Impl(struct type t = [`Env] end)(Sexp)
end

module Langsxp = struct
  include Sxp.Impl(struct type t = [`Lang] end)(Sexp)
end

module type Vector = sig
  type t
  type repr
  include SXP with type t := t
  val length : t -> int
  val of_array : repr array -> t
  val of_list : repr list -> t
  val to_array : t -> repr array
  val to_list : t -> repr list
end

module type Atomic_vector = sig
  include Vector
  val of_array_opt : repr option array -> t
  val to_array_opt : t -> repr option array
end

module type Vector_ops = sig
  type t = private [< vector]
  type repr
  val access : t sxp -> int -> repr
  val alloc : int -> t sxp
  val assign : t sxp -> int -> repr -> unit
end

module type Atomic_vector_ops = sig
  include Vector_ops
  val access_opt : t sxp -> int -> repr option
  val assign_opt : t sxp -> int -> repr option -> unit
end

module Vector_impl(K : Vector_ops) = struct
  include Sxp.Impl(struct type t = K.t end)(Sexp)
  let length (x : t) = length_of_vector x
  let to_list = list_of_vector K.access
  let of_list = vector_of_list K.alloc K.assign
  let to_array = array_of_vector K.access
  let of_array = vector_of_array K.alloc K.assign
end

module Atomic_vector_impl(K : Atomic_vector_ops) = struct
  include Vector_impl(K)
  let to_array_opt = array_of_vector K.access_opt
  let of_array_opt = vector_of_array K.alloc K.assign_opt
end

module Intsxp = struct
  module Elt = struct
    type t = [`Int]
    type repr = int
    let access = access_intsxp
    let access_opt = access_intsxp_opt
    let alloc = alloc_intsxp
    let assign = assign_intsxp
    let assign_opt = assign_intsxp_opt
  end
  include Atomic_vector_impl(Elt)
end

module Realsxp = struct
  module Elt = struct
    type t = [`Real]
    type repr = float
    let access = access_realsxp
    let access_opt = access_realsxp_opt
    let alloc = alloc_real_vector
    let assign = assign_realsxp
    let assign_opt = assign_realsxp_opt
  end
  include Atomic_vector_impl(Elt)
end

module Lglsxp = struct
  module Elt = struct
    type t = [`Lgl]
    type repr = bool
    let access = access_lglsxp
    let access_opt = access_lglsxp_opt
    let alloc = alloc_lglsxp
    let assign = assign_lglsxp
    let assign_opt = assign_lglsxp_opt
  end
  include Atomic_vector_impl(Elt)
end

module Strsxp = struct
  module Elt = struct
    type t = [`Str]
    type repr = string
    let access = access_strsxp
    let access_opt = access_strsxp_opt
    let alloc = alloc_str_vector
    let assign = assign_strsxp
    let assign_opt = assign_strsxp_opt
  end
  include Atomic_vector_impl(Elt)
end

module Vecsxp = struct
  module Elt = struct
    type t = [`Vec]
    type repr = sexp
    let access = access_vecsxp
    let alloc = alloc_vecsxp
    let assign = assign_vecsxp
  end
  include Vector_impl(Elt)
end

module type Conversion = sig
  type 'a t
  val sexp         : sexp t
  val int          : int t
  val ints         : int array t
  val maybe_int    : int option t
  val bool         : bool t
  val bools        : bool array t
  val maybe_bool   : bool option t
  val float        : float t
  val floats       : float array t
  val maybe_float  : float option t
  val string       : string t
  val strings      : string array t
  val maybe_string : string option t
end

module Enc = struct
  type 'a t = 'a -> sexp
  let sexp x = x
  let int i = (Intsxp.of_array [| i |] :> sexp)
  let ints x = (Intsxp.of_array x :> sexp)
  let maybe_int i = (Intsxp.of_array_opt [| i |] :> sexp)
  let bool i = (Lglsxp.of_array [| i |] :> sexp)
  let bools x = (Lglsxp.of_array x :> sexp)
  let maybe_bool i = (Lglsxp.of_array_opt [| i |] :> sexp)
  let float i = (Realsxp.of_array [| i |] :> sexp)
  let floats x = (Realsxp.of_array x :> sexp)
  let maybe_float i = (Realsxp.of_array_opt [| i |] :> sexp)
  let string i = (Strsxp.of_array [| i |] :> sexp)
  let strings x = (Strsxp.of_array x :> sexp)
  let maybe_string i = (Strsxp.of_array_opt [| i |] :> sexp)
end

module Dec = struct
  type 'a t = sexp -> 'a
  let sexp x = x
  let int i = Intsxp.(to_array (unsafe_of_sexp i)).(0)
  let ints x = Intsxp.(to_array (unsafe_of_sexp x))
  let maybe_int i = Intsxp.(to_array_opt (unsafe_of_sexp i)).(0)
  let bool i = Lglsxp.(to_array (unsafe_of_sexp i)).(0)
  let bools x = Lglsxp.(to_array (unsafe_of_sexp x))
  let maybe_bool i = Lglsxp.(to_array_opt (unsafe_of_sexp i)).(0)
  let float i = Realsxp.(to_array (unsafe_of_sexp i)).(0)
  let floats x = Realsxp.(to_array (unsafe_of_sexp x))
  let maybe_float i = Realsxp.(to_array_opt (unsafe_of_sexp i)).(0)
  let string i = Strsxp.(to_array (unsafe_of_sexp i)).(0)
  let strings x = Strsxp.(to_array (unsafe_of_sexp x))
  let maybe_string i = Strsxp.(to_array_opt (unsafe_of_sexp i)).(0)
end


(* === REDUCTION ===== *)


(* The following exception needs to be registered
   in a callback when the R interpreter is initialised. *)
exception Runtime_error of langsxp * string

module Eval = struct
  let string s = eval_langsxp (parse s)
  (* TODO: May segfault if stumbling on a symbol that
   * hasn't yet been loaded. *)

  let rec prepare_args = function
    | (Some x)::l -> x::(prepare_args l)
    | None::l     -> prepare_args l
    | []          -> []

  type arg = (string option * sexp) option
  let arg f ?name x = Some (name, f x)
  let opt_arg f name x = match x with
    | None -> None
    | Some x -> Some ((Some name), f x)

  let call phi (args: (string option * sexp) option list) =
    eval_langsxp (langsxp phi (prepare_args args))
end


(* === INITIALISATION ===== *)

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

module Standard_environment = Standard_environment

external initialise : string array -> int -> int = "ocamlr_initEmbeddedR" [@@noalloc]
external terminate : unit -> unit = "ocamlr_endEmbeddedR" [@@noalloc]

exception Initialisation_failed

let init ?(name     = try Sys.argv.(0) with _ -> "OCaml-R")
         ?(argv     = try List.tl (Array.to_list Sys.argv) with _ -> [])
         ?(env      = Standard_environment.env)
         ?(packages = None)
         ?(sigs     = Standard_environment.signal_handlers) () =
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
        (Runtime_error ((upcast ((null_creator ()) : nilsxp :> sexp ) : langsxp ), ""))
  | _ -> raise Initialisation_failed

module Interpreter_initialization (Env : Environment) : sig end = struct

  let () = init ~name: Env.name
                ~argv: Env.options
                ~env:  Env.env
                ~packages: Env.packages
                ~sigs: Env.signal_handlers
                ()

  let () = at_exit terminate

end
