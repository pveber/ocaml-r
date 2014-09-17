val length : < length : int ; .. > R.t -> R.integer R.t

val subset2_s : < subset2_s : 'b. string -> 'b ; .. > R.t -> R.string_ R.t -> 'a R.t
val subset2_i : < subset2_i : 'b. int -> 'b ; .. > R.t -> R.integer R.t -> 'a R.t

val rle : (_ #R.atomic_vector as 'a) R.t -> < lengths : R.integers R.t ; values : 'a R.t > R.list_ R.t
