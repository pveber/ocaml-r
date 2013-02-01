(** Runtime R statistics library. *)
open Rbase

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


(** {5 Testing} *)
val fisher_test_2x2 : 
  ?alternative:[`two_sided | `greater | `less] ->
  ff:int -> ft:int -> tf:int -> tt:int -> unit -> 
  < p'value : float R.t ;
    conf'int : float list R.t ;
    estimate : float R.t ;
    null'value : float R.t ;
    alternative : string R.t ;
    _method : string R.t ;
    data'name : string R.t > listing R.t

val p'adjust :
  ?method_ : [`holm | `hochberg | `hommel | `bonferroni | `BH | `BY | `fdr] ->
  float list -> float list
