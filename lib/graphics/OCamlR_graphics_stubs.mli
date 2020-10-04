open OCamlR

val plot :
  ?main:R.Sexp.t ->
  ?xlab:R.Sexp.t ->
  ?ylab:R.Sexp.t ->
  ?xlim:R.Sexp.t ->
  ?ylim:R.Sexp.t ->
  ?y:R.Sexp.t ->
  R.Sexp.t -> R.Sexp.t

val plot2 :
  ?main:R.Sexp.t ->
  ?xlab:R.Sexp.t ->
  ?ylab:R.Sexp.t ->
  ?xlim:R.Sexp.t ->
  ?ylim:R.Sexp.t ->
  R.Sexp.t -> R.Sexp.t -> R.Sexp.t

val par :
  ?mfrow:R.Sexp.t ->
  unit -> R.Sexp.t
