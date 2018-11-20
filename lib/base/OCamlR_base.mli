(**  Runtime R base library. *)

open OCamlR

module Environment : sig
  type t

  val create : unit -> t
  (** wrapper for [new.env] *)
end

module Dataframe : sig
  type t
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
