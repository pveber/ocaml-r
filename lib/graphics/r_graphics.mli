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

type plot_type = [
  | `Points
  | `Lines
  | `Both
  | `Overplotted
  | `Histogram
  | `Stair_steps
  | `Other_steps
  | `Nothing
]

type log_scale = [ `X | `Y | `XY ]

val plot :
  ?main:string ->
  ?xlab:string ->
  ?ylab:string ->
  ?xlim:(float * float) ->
  ?ylim:(float * float) ->
  ?plot_type:plot_type ->
  ?lwd:int ->
  ?col:string ->
  ?log:log_scale ->
  x:float list ->
  ?y:float list ->
  unit -> unit

val par :
  ?mfrow:(int * int) ->
  unit -> unit
