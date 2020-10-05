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

(** Bindings for the R interpreter.

    It encapsulates the functionalities
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

(** {3 Low-level SEXP typing.} *)

type 'a sxp = private sexp

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

type 'a pairlist = [< `Nil | `List] as 'a

type vector = [ `Char | `Lgl | `Int  | `Real
              | `Str  | `Raw | `Expr | `Vec]

module Sexp : sig
  type t

  val equal : t -> t -> bool
  val is_function : t -> bool
  val attr : t -> string -> sexp
  val _class_ : t -> string list
  val nil_map : t -> f:(t -> 'a) -> 'a option
  val print : t -> unit
end

module type SXP = sig
  include module type of Sexp
  external unsafe_of_sexp : Sexp.t -> t = "%identity"
  external to_sexp : t -> Sexp.t = "%identity"
end

module Nilsxp : sig
  include SXP with type t = nilsxp
  val create : unit -> t
end

module Dotsxp : sig
  include SXP with type t = dotsxp
  val create : unit -> t
end

module Envsxp : sig
  include SXP with type t = envsxp
end

module Langsxp : sig
  include SXP with type t = langsxp
end

module Symsxp : sig
  include SXP with type t = symsxp

  (** Semantic description of [SYMSXP] structures. *)
  type description = (string * (sexp option)) option option
  val description : t -> description
end

(** {3 Vector types}

    R is an array-oriented language. Therefore, simple values such as
   a boolean, a string, a number, are in fact encapsulated, in R, in
   an array of booleans, an array of strings, an array of numbers. For
   a simple value, the array has only one element.  *)

module type Vector = sig
  include SXP
  type repr
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

(** R array of integer values *)
module Intsxp : Atomic_vector with type t = intsxp
                               and type repr := int

(** R array of boolean values *)
module Lglsxp : Atomic_vector with type t = lglsxp
                               and type repr := bool

(** R array of float values *)
module Realsxp : Atomic_vector with type t = realsxp
                                and type repr := float

(** R array of string values *)
module Strsxp : Atomic_vector with type t = strsxp
                               and type repr := string

(** R list *)
module Vecsxp : Vector with type t = vecsxp
                        and type repr := Sexp.t


(** {2 Value inspection} *)

(**  Algebraic datatype reflecting R's dynamic typing. *)
module Sexptype : sig
  type t =
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

  val of_sexp : sexp -> t
  (**  Returns the R dynamic typing of a wrapped R value. *)

  val to_string : t -> string
end

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
  val t_of_sexp : Sexp.t -> t

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

module type Conversion = sig
  type 'a t
  val sexp         : Sexp.t t
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

module Enc : Conversion with type 'a t = 'a -> Sexp.t
module Dec : Conversion with type 'a t = Sexp.t -> 'a

val symbol : ?generic:bool -> string -> Sexp.t
(**  Retrieves an R symbol from the symbol table, given its name. *)

module Eval : sig
  val string : string -> Sexp.t
  (**  [string] takes a string containing R code, and feeds it to the
       R interpreter. You get the resulting value back. The typing of this
       function is deliberately unsafe in order to allow the user to type
       it precisely.
    *
       Bug: currently, if you try to execute a statement that refers to
       symbols that haven't been loaded, you get a segfault. For instance,
       evaluating a string containing the [rbinom] symbol without the
       [R.stats] package being loaded raises a segfault. *)

  (** Convenience functions to wrap up arguments, when mapping R
     functions to Objective Caml functions. *)
  type arg
  val arg     : 'a Enc.t -> ?name:string -> 'a  -> arg
  val opt_arg : 'a Enc.t -> string -> 'a option -> arg

  val call : Sexp.t -> arg list -> Sexp.t
  (**  [eval f args] evaluates an the R function [f] with respect to a list of
       arguments. Argument [None] is ignored, and [Some (name, sexp)] is the
       argument whose optional name is [name] and whose value is [sexp]. The
       typing of this function is deliberately unsafe in order to allow the
       user to type it precisely. *)
end

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

module Standard_environment : Environment
(**  The [Standard] module contains initialisation details for libR.so.
     These informations are determined when the binding is being compiled.
*)

module Interpreter_initialization(Env : Environment) : sig end
(**  Functor used to initialise statically an R interpreter, given initialisation
     details provided by the provided [Env] module.
*)


(**  {2 Low-level inspection} *)
module Low_level : sig
  external s3_class : sexp -> strsxp = "ocamlr_s3_class"
  external get_attributes : sexp -> _ pairlist sxp = "ocamlr_get_attributes"
  external is_s4_object : sexp -> bool = "ocamlr_is_s4_object"
  external do_new_object : sexp -> sexp = "ocamlr_do_new_object"

  external inspect_attributes : sexp   -> sexp = "ocamlr_inspect_attributes"
  external length_of_vector   : [< vector] sxp -> int  = "ocamlr_inspect_vecsxp_length"

  external inspect_primsxp_offset  : [< `Special | `Builtin ] sxp -> int = "ocamlr_inspect_primsxp_offset"
  external inspect_symsxp_pname    : symsxp         -> sexp              = "ocamlr_inspect_symsxp_pname"
  external inspect_symsxp_value    : symsxp         -> sexp              = "ocamlr_inspect_symsxp_value"
  external inspect_symsxp_internal : symsxp         -> sexp              = "ocamlr_inspect_symsxp_internal"
  external inspect_listsxp_carval  : 'a nonempty_list sxp -> sexp    = "ocamlr_inspect_listsxp_carval"
  external inspect_listsxp_cdrval  : 'a nonempty_list sxp -> [> internallist] sxp = "ocamlr_inspect_listsxp_cdrval"
  external inspect_listsxp_tagval  : 'a nonempty_list sxp -> sexp    = "ocamlr_inspect_listsxp_tagval"
  external inspect_envsxp_frame    : envsxp         -> sexp              = "ocamlr_inspect_envsxp_frame"
  external inspect_envsxp_enclos   : envsxp         -> sexp              = "ocamlr_inspect_envsxp_enclos"
  external inspect_envsxp_hashtab  : envsxp         -> sexp              = "ocamlr_inspect_envsxp_hashtab"
  external inspect_closxp_formals  : closxp         -> sexp              = "ocamlr_inspect_closxp_formals"
  external inspect_closxp_body     : closxp         -> sexp              = "ocamlr_inspect_closxp_body"
  external inspect_closxp_env      : closxp         -> sexp              = "ocamlr_inspect_closxp_env"
  external inspect_promsxp_value   : promsxp        -> sexp              = "ocamlr_inspect_promsxp_value"
  external inspect_promsxp_expr    : promsxp        -> sexp              = "ocamlr_inspect_promsxp_expr"
  external inspect_promsxp_env     : promsxp        -> sexp               = "ocamlr_inspect_promsxp_env"

  external access_lglsxp  : lglsxp  -> int -> bool     = "ocamlr_access_lglsxp"
  external access_intsxp  : intsxp  -> int -> int      = "ocamlr_access_intsxp"
  external access_realsxp : realsxp -> int -> float    = "ocamlr_access_realsxp"
  external access_strsxp  : strsxp  -> int -> string   = "ocamlr_access_strsxp"
  external access_rawsxp  : rawsxp  -> int -> sexp     = "ocamlr_access_vecsxp"
  external access_exprsxp : exprsxp -> int -> langsxp  = "ocamlr_access_vecsxp"

  external null_creator : unit -> [`Nil] sxp = "ocamlr_null"
  external dots_symbol_creator : unit -> [`Dot] sxp = "ocamlr_dots_symbol"
  external missing_arg_creator : unit -> sexp = "ocamlr_missing_arg"
  external base_env_creator : unit -> sexp = "ocamlr_base_env"

  external global_env : unit -> sexp = "ocamlr_global_env"

  val eval_langsxp : langsxp -> sexp
  (**  [eval_langsxp] takes a R value containing an R executable expression.
       Also known as a [LANGSXP]. You get the resulting value back. *)

  val lglsxp_of_bool_list : bool list -> lglsxp
  val intsxp_of_int_list : int list -> intsxp
  val realsxp_of_float_list : float list -> realsxp
  val strsxp_of_string_list : string list -> strsxp
  val realsxp_of_float_option_list : float option list -> realsxp

  val sexps_of_t : rawsxp -> sexp list
  (**  Converts an R array of SEXPs into an OCaml array of
       SEXPs.
  *)

  val classes : sexp -> string list
end

val attributes : sexp -> (Symsxp.description * sexp) list
val pairlist_of_list : (sexp * sexp) list -> [> internallist] sxp
