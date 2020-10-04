open OCamlR

val plot :
  ?main:R.sexp ->
  ?xlab:R.sexp ->
  ?ylab:R.sexp ->
  ?xlim:R.sexp ->
  ?ylim:R.sexp ->
  ?y:R.sexp ->
  R.sexp -> R.sexp

val plot2 :
  ?main:R.sexp ->
  ?xlab:R.sexp ->
  ?ylab:R.sexp ->
  ?xlim:R.sexp ->
  ?ylim:R.sexp ->
  R.sexp -> R.sexp -> R.sexp

val par :
  ?mfrow:R.sexp ->
  unit -> R.sexp
