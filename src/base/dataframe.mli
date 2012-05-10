(**  Virtual class for R data frame S3 objects. *)
class virtual dataframe : object
  inherit Listing.listing
  method row_names : string list
  method column : 'a. int -> 'a R.t
  method element : 'a. int -> int -> 'a R.t
end

val dataframe : dataframe R.t -> dataframe
