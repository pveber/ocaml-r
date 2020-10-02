(* ***************************************************************************** *)
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
(* ***************************************************************************** *)

(** Binding for the R interpreter. It encapsulates the functionalities
    of the [libR.so] shared library provided by the R software. This
    enables us to {b embed the R interpreter} into Objective Caml, to
    execute R code from Objective Caml and to exchange data structures
    between R and Objective Caml.

    {4 THREAD SAFETY}

    It is important to understand that this binding is a rather
    low-level binding of R functionality. As such, it is no more
    thread-safe than R itself, which is not thread-safe at
    all. Therefore, avoid real threading unless you know what you're
    doing...

    {4 DATA CONVERSION}

    R is an array-oriented language. Therefore, simple values such as
    a boolean, a string, a number, are in fact encapsulated, in R, in
    an array of booleans, an array of strings, an array of
    numbers. For a simple value, the array has only one element.

    Moreover, as R is scientific software, it is important that data
    types be correctly matched between Objective Caml and R. At the
    moment, they are not. I am thinking here of the 31/32 bit issues,
    or 63/64 bit issue, or, for instance.
*)


(** {2 Internal representation of R values.} *)

type sexp

type +'a t = private sexp
(** Phantom-typed representation of R values. ['a] provides an
    information on the actual type of the underlying R value. *)

external cast : sexp -> 'a t = "%identity"
(** Upcast an SEXP to any typed representation. Of course this should
    never be used. *)

(**  Algebraic datatype reflecting R's dynamic typing. *)
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

val sexptype : sexp -> sexptype
(**  Returns the R dynamic typing of a wrapped R value. *)


(** {2 Low-level SEXP typing.} *)

type nilsxp         = [`Nil]                                      t
type symsxp         = [`Sym]                                      t
type 'a listsxp     = [`List of [< `Pair | `Call ] as 'a]         t
and 'a internallist = [`Nil | `List of [< `Pair | `Call] as 'a]   t
type langsxp        = [`List of [`Call]]                          t
type closxp         = [`Clo]                                      t
type envsxp         = [`Env]                                      t
type promsxp        = [`Prom]                                     t
type builtinsxp     = [`Builtin]                                  t
type charsxp     = [`Vec  of [`Char]]                          t
type lglsxp      = [`Vec  of [`Lgl ]]                          t
type intsxp      = [`Vec  of [`Int ]]                          t
type realsxp     = [`Vec  of [`Real]]                          t
type strsxp      = [`Vec  of [`Str ]]                          t
type rawsxp      = [`Vec  of [`Raw ]]                          t
type exprsxp     = [`Vec  of [`Raw ]]                          t
type pairlist       = [`Nil | `List of [`Pair]]                   t
type 'a vecsxp      = [`Vec  of
                         [< `Char | `Lgl | `Int  | `Real
                         | `Str  | `Raw | `Expr ] as 'a
                      ] t



(** {2 High-level SEXP typing.} *)

class type ['a] ty = object
  method repr : 'a
end
(** *)

type _ scalar_format =
  | Integer : int scalar_format
  | Real : float scalar_format
  | Logical : bool scalar_format
  | String : string scalar_format

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


(** {2 Symbol retrieval.} *)

val symbol : ?generic:bool -> string -> symsxp
(**  Retrieves an R symbol from the symbol table, given its name. *)

(** {2 Conversion functions.} *)

val bools_of_t : logicals t -> bool array
(** Converts an R array of logical values into an array of OCaml
    booleans.  *)

val bool_of_t : logical t -> bool
(**  Converts an R array of logical values with one element into an
     Objective Caml boolean.
*)

val bool : bool -> logical t
(**  Converts an Objective Caml boolean value to an R boolean value,
     that is a mono-element array of booleans.
*)

val bools : bool array -> logicals t
(** Converts an OCaml array of booleans into an R array of logical
    values.  *)

val ints_of_t : integers t -> int array
(** Converts an R array of integers into an array of OCaml
    integers.  *)

val optints_of_t : integers t -> int option array
(** Converts an R array of integers possibly containing [NA] values to
   optional ints. *)

val int_of_t : integer t -> int
(**  Converts an R array of integers with one element into an Objective
     Caml integer.
*)

val int : int -> integer t
(**  Converts an Objective Caml integer to an R integer value, that
     is a mono-element array of integers.
*)

val ints : int array -> integers t
(** Converts an OCaml array of integers into an R array of
    integers.  *)

val optints : int option array -> integers t
(**  Converts a OCaml array of int options into an R array of
     integer numbers with possibly missing values. The value [None] is
     converted to [NA] on the R side.
*)

val floats_of_t : reals t -> float array
(** Converts an R array of real numbers into an array of OCaml
    floats.  *)

val optfloats_of_t : reals t -> float option array
(** Converts an R array of floats possibly containing [NA] values to
   optional floats. *)

val float_of_t : real t -> float
(** Converts an R array of floats with one element into an OCaml
    float.  *)

val float : float -> real t
(**  Converts an OCaml float to an R real value, that is a
     mono-element array of real numbers.
*)

val floats : float array -> reals t
(**  Converts an OCaml array of floats into an R array of
     real numbers.
*)

val optfloats : float option array -> reals t
(**  Converts a OCaml array of float options into an R array of
     real numbers with possibly missing values. The value [None] is
     converted to [NA] on the R side.
*)

val strings_of_t : strings t -> string array
(** Converts an R array of strings into a list of OCaml strings. *)

val string_of_t : string_ t -> string
(** Converts an R array of strings with one element into an OCaml
    string.  *)

val string : string -> string_ t
(**  Converts an OCaml string to an R string, that is a
     mono-element array of strings.
*)

val strings : string array -> strings t
(**  Converts an OCaml array of strings into an R array of
     strings.
*)

val sexps_of_t : rawsxp -> sexp list
(**  Converts an R array of SEXPs into an OCaml array of
     SEXPs.
*)


(**  {2 Inspection and specification of internals.} *)

module Specification : sig
  (** Semantic description of [SYMSXP] structures. *)
  type symbol = (string * (sexp option)) option option
end

val is_nil : _ t -> bool
val nil_map : _ t -> f:(_ t -> 'a) -> 'a option
val notnil : 'a t -> 'a t option

val attributes : sexp -> (Specification.symbol * sexp) list

val classes : sexp -> string list

(**  Provides facilities to inspect internal structure of
     SEXPs. Useful in the toplevel when you encounter
     unexpected R values. *)
module Pretty : sig

  (**  Semantic interpretation and description of SEXPs. *)
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
  and environment = { frame: t }
  and promise     = { value: t; expr: t; prom_env: t }

  and pairlist = (t * t) list

  (**  Analyses recursively the structure of a given SEXP. *)
  val t_of_sexp : sexp -> t

end


(** {2 Parsing R code.} *)

type parse_status =
  | Parse_Null
  | Parse_OK
  | Parse_Incomplete
  | Parse_Error
  | Parse_EOF
  (**  Outcome of a parsing request. *)

exception Parsing_failure of parse_status * string
(**  Exception raised when parsing fails. *)

val parse_string : ?max:int -> string -> langsxp list
(**  Parse a string of R code into R calls.

     @param max If omitted, parse the whole R code, even if
     there are multiple statements. Otherwise, maximum
     number of statements to parse.
     @raise Parsing_failure When parsing fails. *)

val parse : string -> langsxp
(**  Parse the first R statement in the given R code. *)


(** {2 Evaluation of R code and calls.} *)

exception Runtime_error of langsxp * string

val eval_langsxp : langsxp -> 'a t
(**  [eval_langsxp] takes a R value containing an R executable expression.
     Also known as a [LANGSXP]. You get the resulting value back. *)

val eval_string : string -> 'a t
(**  [eval_string] takes a string containing R code, and feeds it to the
     R interpreter. You get the resulting value back. The typing of this
     function is deliberately unsafe in order to allow the user to type
     it precisely.
  *
     Bug: currently, if you try to execute a statement that refers to
     symbols that haven't been loaded, you get a segfault. For instance,
     evaluating a string containing the [rbinom] symbol without the
     [R.stats] package being loaded raises a segfault. *)

val arg : ('a -> 'b t) -> ?name:string -> 'a -> (string option * sexp) option
(**  Convenience function to wrap up arguments, when mapping R functions
     to Objective Caml functions. *)

val opt : ('a -> 'b t) -> string -> 'a option -> (string option * sexp) option
(**  Convenience function to wrap up optional arguments, when mapping R functions
     to Objective Caml functions. *)

val eval : symsxp -> (string option * sexp) option list -> 'a t
(**  [eval f args] evaluates an the R function [f] with respect to a list of
     arguments. Argument [None] is ignored, and [Some (name, sexp)] is the
     argument whose optional name is [name] and whose value is [sexp]. The
     typing of this function is deliberately unsafe in order to allow the
     user to type it precisely. *)


(** {2 Initialisation}

    We provide two mechanisms to activate an R interpreter from OCaml-R:

    The first mechanism consists of low-level bindings to the
    initialisation and termination functions of the [libR.so] shared
    library. While this is a rather flexible approach, it has the
    downside of not being a very static approach, specifically if your
    intention if to write Objective Caml bindings for a dependent
    bunch of R packages.

    The second mechanism is a static, functorial approach: You just
    have to create a module with the [Interpreter] functor to
    initialise the R interpreter.  You provide initialisation details
    through a module of module type [Environment], and [Interpreter]
    will set it up correctly.

    This functorial facility is available from the OCaml_R module:
    This OCaml_R module has the sole purpose of initialising the R
    interpreter with the [Standard] [Environment] module. No need to
    worry about initialisation details.

    To create bindings for a dependent bunch of R packages, you simply
    have to make them depend on the findlib [R.interpreter] package,
    which involves the OCaml_R module. This is also convenient on the
    toplevel, where you simply have to have to invoke the
    [#require "R.interpreter"] directive to set up the interpreter.
*)

exception Initialisation_failed
(**  Denotes failure to initialise the R interpreter. *)

val init : ?name:string -> ?argv:string list -> ?env:(string * string) list -> ?packages:string list option -> ?sigs:bool -> unit -> unit
(**  [init] initialises the embedded R interpreter.

     @param name Name of program. Defaults to Sys.argv.(0).
     @param argv Command line options given to [libR.so]. Defaults to rest of Sys.argv.
     @param env Environment variables to be set for R. Defaults to reasonable values.
     @param packages Packages to be loaded at startup. If [None], load the usual standard library.
     @param sigs If [false], stops R from setting his signal handlers. Defaults to [false].
     @raise Initialisation_failed In case R could not be started. *)

val terminate : unit -> unit
(**  Terminates the R session. *)

module type Interpreter = sig end
(**  Module type of an R interpreter. *)

module type Environment = sig
  (**  [Environment] is the type of a module containing all necessary
       informations and data in order to set up the R interpreter
       properly. *)

  val name : string
  (**  This is the [name] of the first argument of [argv] for R.
       Mandatory, otherwise [libR.so] segfaults immediately. *)

  val options : string list
  (**  Other command line options passed to the [libR.so] library when
       initialising R.

       @see "R reference manual" File refman.pdf, page 452, section intitled
       'Startup - Initialization at Start of an R Session' for details
       about the most important command line options.
       @see <http://cran.r-project.org/doc/manuals/R-intro.html#Invoking-R>
       For command line options.
  *)

  val signal_handlers : bool
  (**  If set to [false], asks R not to install its signal handlers. I've
       been experiencing weird issues with R signal handlers, since, for
       instance, a [SIGSEGV] is sometimes caught by [libR.so], and R then
       asks whether or not you want to save your workspace, et ceteræ... By
       default, set to false.
  *)

  val env : (string * string) list
  (**  These are environment variables that needs to be set before
       initialising the R interpreter. In the [Standard] module, these
       values are determined when the binding itself is compiled.
  *)

  val packages : string list option
  (**  Packages loaded on startup. If set to [None], load the usual standard
       library. Otherwise, if set to [Some p], load packages [p] in place of
       the standard library. In the [Standard] module, this is set to [Some []]. *)

end

module Standard : Environment
(**  The [Standard] module contains initialisation details for libR.so.
     These informations are determined when the binding is being compiled.
*)

module Interpreter (Env : Environment) : Interpreter
(**  Functor used to initialise statically an R interpreter, given initialisation
     details provided by the provided [Env] module.
*)


(**  {2 Low-level inspection} *)

external s3_class : sexp -> sexp = "ocamlr_s3_class"
external get_attributes : sexp -> pairlist = "ocamlr_get_attributes"
external is_s4_object : sexp -> bool = "ocamlr_is_s4_object"
external do_new_object : sexp -> sexp = "ocamlr_do_new_object"

external inspect_attributes : sexp   -> sexp = "ocamlr_inspect_attributes"
external length_of_vecsxp   : 'a vecsxp -> int  = "ocamlr_inspect_vecsxp_length"

external inspect_primsxp_offset  : [< `Special | `Builtin ] t -> int = "ocamlr_inspect_primsxp_offset"
external inspect_symsxp_pname    : symsxp         -> sexp            = "ocamlr_inspect_symsxp_pname"
external inspect_symsxp_value    : symsxp         -> sexp            = "ocamlr_inspect_symsxp_value"
external inspect_symsxp_internal : symsxp         -> sexp            = "ocamlr_inspect_symsxp_internal"
external inspect_listsxp_carval  : 'a listsxp     -> sexp            = "ocamlr_inspect_listsxp_carval"
external inspect_listsxp_cdrval  : 'a listsxp     -> sexp            = "ocamlr_inspect_listsxp_cdrval"
external inspect_listsxp_tagval  : 'a listsxp     -> sexp            = "ocamlr_inspect_listsxp_tagval"
external inspect_envsxp_frame    : envsxp         -> sexp            = "ocamlr_inspect_envsxp_frame"
external inspect_envsxp_enclos   : envsxp         -> sexp            = "ocamlr_inspect_envsxp_enclos"
external inspect_envsxp_hashtab  : envsxp         -> sexp            = "ocamlr_inspect_envsxp_hashtab"
external inspect_closxp_formals  : closxp         -> sexp            = "ocamlr_inspect_closxp_formals"
external inspect_closxp_body     : closxp         -> sexp            = "ocamlr_inspect_closxp_body"
external inspect_closxp_env      : closxp         -> sexp            = "ocamlr_inspect_closxp_env"
external inspect_promsxp_value   : promsxp        -> sexp            = "ocamlr_inspect_promsxp_value"
external inspect_promsxp_expr    : promsxp        -> sexp            = "ocamlr_inspect_promsxp_expr"
external inspect_promsxp_env     : promsxp        -> sexp            = "ocamlr_inspect_promsxp_env"

external access_lglsxp  : lglsxp  -> int -> bool     = "ocamlr_access_lglsxp"
external access_intsxp  : intsxp  -> int -> int      = "ocamlr_access_intsxp"
external access_realsxp : realsxp -> int -> float    = "ocamlr_access_realsxp"
external access_strsxp  : strsxp  -> int -> string   = "ocamlr_access_strsxp"
external access_rawsxp  : rawsxp  -> int -> sexp     = "ocamlr_access_vecsxp"
external access_exprsxp : exprsxp -> int -> langsxp  = "ocamlr_access_vecsxp"

val pairlist_of_list : (_ t * _ t) list -> pairlist
val lglsxp_of_bool_list : bool list -> lglsxp
val intsxp_of_int_list : int list -> intsxp
val realsxp_of_float_list : float list -> realsxp
val strsxp_of_string_list : string list -> strsxp
val realsxp_of_float_option_list : float option list -> realsxp

external null_creator : unit -> nilsxp = "ocamlr_null"
external dots_symbol_creator : unit -> sexp = "ocamlr_dots_symbol"
external missing_arg_creator : unit -> sexp = "ocamlr_missing_arg"
external base_env_creator : unit -> sexp = "ocamlr_base_env"

external global_env : unit -> sexp = "ocamlr_global_env"

val string_of_sexptype : sexptype -> string
