(** Virtual class for dates in R. *)
class virtual date : object
  inherit R.s3
  method as_float : float
  method as_date : CalendarLib.Calendar.Date.t
end
