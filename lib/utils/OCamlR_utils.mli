open OCamlR_base

val data :
  ?envir:Environment.t ->
  string -> unit

val write'table :
  ?file:string ->
  ?sep:string ->
  ?col'names:bool ->
  ?row'names:bool ->
  ?quote:bool ->
  Dataframe.t ->
  unit
