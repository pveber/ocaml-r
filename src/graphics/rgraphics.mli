(** Runtime R graphics library. *)

val hist : 
  ?breaks:[`n of int | `l of float list R.t | `m of [`Sturges | `Scott | `FD]] ->
  ?freq:bool ->
  ?include_lowest:bool ->
  ?right:bool ->
  ?main:string -> ?xlab:string -> ?ylab:string ->
  ?xlim:float -> ?ylim:float -> 
  float list R.t -> 'a R.t
