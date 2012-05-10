class virtual date = object (self)
  inherit R.s3
  method as_float = R.float_of_t (Obj.magic __underlying)
  method as_date = CalendarLib.Calendar.Date.from_unixfloat (86400. *. self#as_float)
end
