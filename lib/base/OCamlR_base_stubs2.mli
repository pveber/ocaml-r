open OCamlR

val length : _ R.t -> R.integer R.t

val subset : _ R.t -> R.integer R.t -> 'a R.t
val subset_ii :
  _ R.t ->
  R.integer R.t ->
  R.integer R.t ->
  'b R.t
val subset2_s : _ R.t -> R.string_ R.t -> 'a R.t
val subset2_i : _ R.t -> R.integer R.t -> 'a R.t
val dim : < dim : R.integers R.t ; .. > R.t -> R.integers R.t

module Matrix : sig
  val subset : _ R.t -> R.integer R.t -> _ R.t
  val subset_ii : _ R.t -> R.integer R.t -> R.integer R.t -> 'a R.scalar R.t
  val subset2 : _ R.t -> R.integer R.t -> 'a R.atomic_vector R.t
end

val rle :
  (_ #R.atomic_vector as 'a) R.t ->
  < lengths : R.integers R.t ; values : 'a R.t > R.t

val sample :
  (< length : R.integer R.t ; subset : 'b. R.integer R.t -> 'b R.t ; .. > as 'c) R.t ->
  R.integer R.t ->
  ?replace:R.logical R.t ->
  ?prob:R.reals R.t ->
  unit ->
  'c R.t

val min : 'a #R.atomic_vector R.t -> 'a R.scalar R.t
val max : 'a #R.atomic_vector R.t -> 'a R.scalar R.t
