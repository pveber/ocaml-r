(**  Runtime R base library. *)

open OCamlR

module S3 : sig
  type t

  val r : t -> t R.t
  val _class_ : t -> string array
end

module Environment : sig
  type t
  include module type of S3 with type t := t

  val create : unit -> t
  (** wrapper for [new.env] *)
end

module type Atomic_vector = sig
  type t
  type format
  type elt
  val r : t -> format R.t
  val length : t -> int
  val to_array : t -> elt array
  val of_array : elt array -> t
end

module Numeric : Atomic_vector with type elt = float and type format = R.reals
module Logical : Atomic_vector with type elt = bool and type format = R.logicals
module Integer : Atomic_vector with type elt = int and type format = R.integers
module Character : Atomic_vector with type elt = string and type format = R.strings
module Factor : Atomic_vector with type elt = string and type format = R.strings

module Dataframe : sig
  type t
  include module type of S3 with type t := t

  val of_env : Environment.t -> string -> t option
  val dim : t -> int * int
end

val sample :
  ?replace:bool ->
  ?prob:float array ->
  size:int ->
  float array ->
  float array

(** {2 Low-level access}

    Use with great care!
*)

val subset : _ R.t -> int -> 'b R.t
val subset_ii : _ R.t -> int -> int -> 'b R.t
val subset2_s : _ R.t -> string -> 'b R.t
val subset2_i : _ R.t -> int -> 'b R.t
