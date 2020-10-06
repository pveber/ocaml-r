(**  Runtime R base library. *)

open OCamlR

module Environment : sig
  include SXP

  val create : unit -> t
  (** wrapper for [new.env] *)

  val get : t -> class_:string -> string -> Sexp.t option
end

module Numeric : Atomic_vector with type repr := float
module Logical : Atomic_vector with type repr := bool
module Integer : Atomic_vector with type repr := int
module Character : Atomic_vector with type repr := string

module Factor : sig
  include module type of Integer
  val of_integer : Integer.t -> t
  val of_character : Character.t -> t
  val levels : t -> Character.t
end

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

  type column
  val numeric : string -> Numeric.t -> column
  val integer : string -> Integer.t -> column
  val logical : string -> Logical.t -> column
  val character : string -> Character.t -> column
  val factor : string -> Factor.t -> column

  val create : column list -> t
  val rbind : t -> t -> t
  val cbind : t -> t -> t
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

module Matrix : sig
  include module type of Numeric
  val dim : t -> int * int
  val of_arrays : float array array -> t
  val get2 : t -> int -> int -> float
  val get_row : t -> int -> Numeric.t
  val get_col : t -> int -> Numeric.t
end
