open OCamlR

val length : Sexp.t -> Sexp.t

val subset : Sexp.t -> Sexp.t -> Sexp.t
val subset_ii :
  Sexp.t ->
  Sexp.t ->
  Sexp.t ->
  Sexp.t
val subset2_s : Sexp.t -> Sexp.t -> Sexp.t
val subset2_i : Sexp.t -> Sexp.t -> Sexp.t
val dim : Sexp.t -> Sexp.t

module Matrix : sig
  val subset : Sexp.t -> Sexp.t -> Sexp.t
  val subset_ii : Sexp.t -> Sexp.t -> Sexp.t -> Sexp.t
  val subset2 : Sexp.t -> Sexp.t -> Sexp.t
end

val rle : Sexp.t -> Sexp.t

val sample :
  Sexp.t ->
  Sexp.t ->
  ?replace:Sexp.t ->
  ?prob:Sexp.t ->
  unit ->
  Sexp.t

val min : Sexp.t -> Sexp.t
val max : Sexp.t -> Sexp.t
