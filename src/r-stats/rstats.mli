(** Runtime R statistics library. *)

val rnorm : ?mean:float -> ?sd:float -> int -> float list R.t
(** Random generation for the normal distribution. [mean] and [sd] default to [0.]
    and [1.] respectively. *)

val cor : 'a R.t -> ?y:'b R.t -> ?use:'c R.t -> ?cor_method:'d R.t -> unit -> 'e R.t
(**  Calculates correlations. *)

val lm : 'a R.t -> ?data:'b R.t -> ?subset:'c R.t -> ?weights:'d R.t ->
  ?na_action:'e R.t -> ?lm_method:'f R.t -> ?model:'g R.t -> ?x:'h R.t ->
  ?y:'i R.t -> ?qr:'j R.t -> ?singular_ok:'k R.t -> ?contrasts:'l R.t ->
  ?offset:'m R.t -> unit -> 'n R.t
(**  Makes a linear regression. *)

(* [stl] Seasonal decomposition of time series by Loess. *)










