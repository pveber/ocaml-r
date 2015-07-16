class type ['a] list_ = object
  inherit ['a] R.s3 constraint 'a = < .. >
  method ty : 'a
  method length : R.integer R.t
  method subset2_s : 'b. R.string_ R.t -> 'b R.t
  method subset2_i : 'b. R.integer R.t -> 'b R.t
end

class type ['a] data'frame  = object
  inherit ['a] list_
  method dim : R.integers R.t
end

