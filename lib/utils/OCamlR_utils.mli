open OCamlR_base

val data :
  ?envir:Environment.t ->
  string -> unit

val read'table :
  ?header:bool ->
  ?sep:string ->
  ?quote:string ->
  ?dec:string ->
  ?numerals:[`allow'loss | `warn'loss | `no'loss] ->
  ?row'names:bool ->
  ?col'names:bool ->
  ?na'strings:string ->
  ?check'names:bool ->
  ?strip'white:bool ->
  ?comment'char:string ->
  ?stringsAsFactors:bool ->
  string ->
  Dataframe.t

val write'table :
  ?file:string ->
  ?sep:string ->
  ?col'names:bool ->
  ?row'names:bool ->
  ?quote:bool ->
  Dataframe.t ->
  unit
