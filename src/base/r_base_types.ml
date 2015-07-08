class type ['a] list_ = object
  inherit ['a] R.s3 constraint 'a = < .. >
  method ty : 'a
  method length : int
  method subset2_s : 'b. string -> 'b R.t
  method subset2_i : 'b. int -> 'b R.t
end

class type ['a] data'frame  = object
  inherit ['a] list_
  method dim : int * int
end

