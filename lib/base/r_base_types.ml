open OCaml_R

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

class type ['a] matrix = object
  inherit ['a] R.atomic_vector
  method dim : R.integers R.t
  method subset : R.integer R.t -> 'a matrix R.t
  method subset_ii : R.integer R.t -> R.integer R.t -> 'a R.scalar R.t
  method subset2 : R.integer R.t -> 'a R.atomic_vector R.t
end
