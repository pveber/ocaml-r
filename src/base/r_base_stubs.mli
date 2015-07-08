open R_base_types

val length : < length : int ; .. > R.t -> R.integer R.t

val subset : < subset : 'b. int -> 'b R.t ; .. > R.t -> R.integer R.t -> 'a R.t
val subset_ii :
  < subset_ii : 'b. int -> int -> 'b R.t ; .. > R.t ->
  R.integer R.t ->
  R.integer R.t ->
  'b R.t
val subset2_s : < subset2_s : 'b. string -> 'b R.t ; .. > R.t -> R.string_ R.t -> 'a R.t
val subset2_i : < subset2_i : 'b. int -> 'b R.t ; .. > R.t -> R.integer R.t -> 'a R.t
val dim : < dim : R.integers R.t ; .. > R.t -> R.integers R.t

val rle :
  (_ #R.atomic_vector as 'a) R.t ->
  < lengths : R.integers R.t ; values : 'a R.t > list_ R.t

val sample :
  (< length : int ; subset : 'b. int -> 'b R.t ; .. > as 'c) R.t ->
  R.integer R.t ->
  ?replace:R.logical R.t ->
  ?prob:R.reals R.t ->
  unit ->
  'c R.t

