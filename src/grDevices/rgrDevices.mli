(** Runtime R grDevices library. *)

type length_unit = [`pixel | `inch | `cm | `mm]

val png : 
  ?width:float -> ?height:float -> 
  ?unit:length_unit ->
  ?pointsize:int -> 
  string -> unit

val dev_off : unit -> unit
