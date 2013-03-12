(**  Runtime R base library. *)

(**  Sampling function. *)
val sample : 'a list R.t -> int -> ?replace: bool -> ?prob: float list -> unit -> 'a list R.t

(**  Lapply function, somewhat like List.map.*)
val lapply : 'a list R.t -> 'b R.t -> 'c list R.t


class array_ : array_ R.t -> object
  inherit R.s3
  method dim : float list R.t
end

class matrix : matrix R.t -> object
  inherit array_
  method floats : float array array
end

val matrix : ?byrow:bool -> nrow:int -> ncol:int -> float list -> matrix R.t

val matrix_by_rows : float list list -> matrix R.t


type 'a compound = private < component : 'b. string -> 'b R.t ; .. >

(**  Virtual class for R list S3 objects. *)
class ['a] listing : 'a listing R.t -> object
  inherit R.s3
  method component : 'b. string -> 'b R.t
  method names : string list
  method compound : 'a compound
end

val listing : 'a listing R.t -> 'a listing

(**  Virtual class for R data frame S3 objects. *)
class ['a] dataframe : 'a dataframe R.t -> object
  inherit ['a] listing
  method row_names : string list
  method column : 'a. int -> 'a R.t
  method element : 'a. int -> int -> 'a R.t
end

val dataframe : 'a dataframe R.t -> 'a dataframe

(** Virtual class for dates in R. *)
class date : date R.t -> object
  inherit R.s3 
  method as_float : float
  method as_date : CalendarLib.Calendar.Date.t
end

module Listing : sig
  type 'a t = [ `listing of (< .. > as 'a) ] compound R.t

  val length : 'a t -> int
  val nth : 'a t -> int -> 'b R.t
  val elt : 'a t -> string -> 'b R.t
end

module Infix : sig
  val ( $ ) : 'a compound R.t -> string -> 'b R.t
end



















