(**  Runtime R base library. *)

open OCamlR

val inherits : Sexp.t -> string -> bool

module Environment : sig
  include SXP

  val create : unit -> t
  (** wrapper for [new.env] *)

  val get : t -> class_:string -> string -> Sexp.t option
end

module type Matrix = sig
  include Atomic_vector
  type vector
  val dim : t -> int * int
  val as_vector : t -> vector
  val of_arrays : repr array array -> t
  val get2 : t -> int -> int -> repr
  val get_row : t -> int -> vector
  val get_col : t -> int -> vector
end

module type Vector = sig
  include Atomic_vector
  module Matrix : Matrix with type repr := repr
                          and type vector := t
end

module Numeric : Vector with type repr := float
module Logical : Vector with type repr := bool
module Integer : Vector with type repr := int
module Character : Vector with type repr := string

module Factor : sig
  include module type of Integer
  val of_integer : Integer.t -> t
  val of_character : Character.t -> t
  val levels : t -> Character.t
end

type matrix = [
  | `Numeric   of Numeric.Matrix.t
  | `Logical   of Logical.Matrix.t
  | `Integer   of Integer.Matrix.t
  | `Factor    of Factor.Matrix.t
  | `Character of Character.Matrix.t
]

module List_ : sig
  include SXP
  val as_vecsxp : t -> Vecsxp.t
  val subset2 : t -> string -> 'a Dec.t -> 'a option
  val subset2_i : t -> int -> 'a Dec.t -> 'a option
  val subset2_exn : t -> string -> 'a Dec.t -> 'a
  val subset2_i_exn : t -> int -> 'a Dec.t -> 'a
end

module Dataframe : sig
  include module type of List_

  val of_env : Environment.t -> string -> t option
  val dim : t -> int * int

  val as_list : t -> List_.t

  type column = [
    | `Numeric of Numeric.t
    | `Integer of Integer.t
    | `Logical of Logical.t
    | `Character of Character.t
    | `Factor of Factor.t
  ]

  val create : (string * column) list -> t
  val rbind : t -> t -> t
  val cbind : t -> t -> t

  val get_row : t -> int -> t
  val get_col : t -> int -> column

  val as'matrix : t -> matrix
end


val sample :
  ?replace:bool ->
  ?prob:float array ->
  size:int ->
  float array ->
  float array

val readRDS : string -> Sexp.t

val saveRDS :
  ?ascii:bool ->
  ?compress:bool ->
  file:string ->
  Sexp.t -> unit
