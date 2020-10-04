open OCamlR.R

val length : sexp -> sexp

val subset : sexp -> sexp -> sexp
val subset_ii :
  sexp ->
  sexp ->
  sexp ->
  sexp
val subset2_s : sexp -> sexp -> sexp
val subset2_i : sexp -> sexp -> sexp
val dim : sexp -> sexp

module Matrix : sig
  val subset : sexp -> sexp -> sexp
  val subset_ii : sexp -> sexp -> sexp -> sexp
  val subset2 : sexp -> sexp -> sexp
end

val rle : sexp -> sexp

val sample :
  sexp ->
  sexp ->
  ?replace:sexp ->
  ?prob:sexp ->
  unit ->
  sexp

val min : sexp -> sexp
val max : sexp -> sexp
