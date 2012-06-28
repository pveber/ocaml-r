(**  Runtime R base library. *)

(**  Sampling function. *)
val sample : 'a list R.t -> int -> ?replace: bool -> ?prob: float list -> unit -> 'a list R.t

(**  Lapply function, somewhat like List.map.*)
val lapply : 'a list R.t -> 'b R.t -> 'c list R.t

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
















