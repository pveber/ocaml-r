(**  Runtime R base library. *)

(**  Sampling function. *)
val sample : 'a list R.t -> int -> ?replace: bool -> ?prob: float list -> unit -> 'a list R.t

(**  Lapply function, somewhat like List.map.*)
val lapply : 'a list R.t -> 'b R.t -> 'c list R.t

include module type of Listing
include module type of Dataframe
include module type of Date











