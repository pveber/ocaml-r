(**  Runtime R base library. *)

(**  Sampling function. *)
val sample : 'a list R.t -> int -> ?replace: bool -> ?prob: float list -> unit -> 'a list R.t

(**  Lapply function, somewhat like List.map.*)
val lapply : 'a list R.t -> 'b R.t -> 'c list R.t

(**  Virtual class for R list S3 objects. *)
class virtual listing : object
  inherit R.s3
  method names : string list
end


(**  Virtual class for R data frame S3 objects. *)
class virtual dataframe : object
  inherit Listing.listing
  method row_names : string list
  method column : 'a. int -> 'a R.t
  method element : 'a. int -> int -> 'a R.t
end

val dataframe : dataframe R.t -> dataframe

(** Virtual class for dates in R. *)
class virtual date : object
  inherit R.s3
  method as_float : float
  method as_date : CalendarLib.Calendar.Date.t
end










