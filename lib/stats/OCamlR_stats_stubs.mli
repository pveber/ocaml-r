open OCamlR
open OCamlR_base_types

val rnorm : ?mean:R.real R.t -> ?sd:R.real R.t -> R.integer R.t -> R.reals R.t

val fisher'test :
  ?alternative:R.string_ R.t ->
  'a #R.atomic_vector R.t ->
  'a #R.atomic_vector R.t ->
  < p'value : R.real R.t ;
    conf'int : R.reals R.t  ;
    estimate : R.real R.t ;
    null'value : R.real R.t ;
    alternative : R.string_ R.t ;
    method_ : R.string_ R.t ;
    data'name : R.string_ R.t > list_ R.t
