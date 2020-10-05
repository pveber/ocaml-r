open OCamlR

val plot :
  ?main:Sexp.t ->
  ?xlab:Sexp.t ->
  ?ylab:Sexp.t ->
  ?xlim:Sexp.t ->
  ?ylim:Sexp.t ->
  ?y:Sexp.t ->
  Sexp.t -> Sexp.t

val plot2 :
  ?main:Sexp.t ->
  ?xlab:Sexp.t ->
  ?ylab:Sexp.t ->
  ?xlim:Sexp.t ->
  ?ylim:Sexp.t ->
  Sexp.t -> Sexp.t -> Sexp.t

val par :
  ?mfrow:Sexp.t ->
  unit -> Sexp.t
