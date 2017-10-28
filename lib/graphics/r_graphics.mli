(** Runtime R graphics library. *)

type hist = < breaks : float list ;
              counts : float list ;
              density : float list ;
              mids : float list ;
              xname : string ;
              equidist : bool >
val hist :
  ?breaks:[`n of int | `l of float list | `m of [`Sturges | `Scott | `FD]] ->
  ?freq:bool ->
  ?include_lowest:bool ->
  ?right:bool ->
  ?main:string -> ?xlab:string -> ?ylab:string ->
  ?xlim:float -> ?ylim:float ->
  ?plot:bool ->
  float list -> hist

val plot :
  ?main:string ->
  ?xlab:string ->
  ?ylab:string ->
  ?xlim:(float * float) ->
  ?ylim:(float * float) ->
  x:float list ->
  ?y:float list ->
  unit -> unit
