open OCamlR

val plot :
  ?main:R.string_ R.t ->
  ?xlab:R.string_ R.t ->
  ?ylab:R.string_ R.t ->
  ?xlim:R.reals R.t ->
  ?ylim:R.reals R.t ->
  ?y:_ R.t ->
  _ R.t -> R.nilsxp

val plot2 :
  ?main:R.string_ R.t ->
  ?xlab:R.string_ R.t ->
  ?ylab:R.string_ R.t ->
  ?xlim:R.reals R.t ->
  ?ylim:R.reals R.t ->
  _ R.t -> _ R.t -> R.nilsxp

val par :
  ?mfrow:_ R.t ->
  unit -> _ R.t
