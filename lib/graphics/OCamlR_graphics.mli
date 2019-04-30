(** Runtime R graphics library. *)

type hist = < breaks : float array ;
              counts : float array ;
              density : float array ;
              mids : float array ;
              xname : string ;
              equidist : bool >
val hist :
  ?breaks:[`n of int | `l of float array | `m of [`Sturges | `Scott | `FD]] ->
  ?freq:bool ->
  ?include_lowest:bool ->
  ?right:bool ->
  ?main:string -> ?xlab:string -> ?ylab:string ->
  ?xlim:float -> ?ylim:float ->
  ?plot:bool ->
  float array -> hist

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
  x:float array ->
  ?y:float array ->
  unit -> unit

val lines :
  ?lwd:int ->
  ?col:string ->
  x:float array ->
  ?y:float array ->
  unit ->
  unit

val par :
  ?mfrow:(int * int) ->
  unit -> unit

val dataframe_boxplot :
  ?main:string ->
  ?xlab:string ->
  ?ylab:string ->
  OCamlR_stats.Formula.t ->
  OCamlR_base.Dataframe.t ->
  unit
