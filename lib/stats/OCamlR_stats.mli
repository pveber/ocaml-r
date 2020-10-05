
(** Runtime R statistics library. *)

open OCamlR_base

module Formula : sig
  include OCamlR.SXP
  val of_string : string -> t
end

module Ecdf : sig
  type t
  val make : Numeric.t -> t
  val plot :
    ?main:string ->
    ?xlab:string ->
    ?ylab:string ->
    ?xlim:(float * float) ->
    ?ylim:(float * float) ->
    t ->
    unit
end

(** {2 Random number generation} *)

val rnorm : ?mean:float -> ?sd:float -> int -> float array
(** Random generation for the normal distribution. [mean] and [sd] default to [0.]
    and [1.] respectively. *)


(** {2 Tests} *)

class type test = object
  method p'value : float
  method method_ : string
  method data'name : string
  method alternative : string
end

val fisher'test :
  ?alternative:[`two_sided | `greater | `less] ->
  Logical.t ->
  Logical.t ->
  < test ;
    conf'int : (float * float) option  ;
    estimate : float ;
    null'value : float >

(* val fisher'test_2x2 :
 *   ?alternative:[`two_sided | `greater | `less] ->
 *   ff:int -> ft:int -> tf:int -> tt:int -> unit ->
 *   < p'value : float ;
 *     conf'int : (float * float) option  ;
 *     estimate : float ;
 *     null'value : float ;
 *     alternative : string ;
 *     method_ : string ;
 *     data'name : string > *)

val chisq'test_contingency_table :
  ?correct:bool ->
  ?simulate'p'value:bool ->
  ?b:int ->
  Matrix.t ->
  < test ; statistic : float >

val ks'test :
  ?alternative:[`two_sided | `greater | `less] ->
  float array -> float array ->
  < test ; statistic : float >

val p'adjust :
  ?method_ : [`holm | `hochberg | `hommel | `bonferroni | `BH | `BY | `fdr] ->
  float array -> float array




(* module Stub : sig *)



(*   (\** {5 Misc} *\) *)

(*   val cor : 'a R.t -> ?y:'b R.t -> ?use:'c R.t -> ?cor_method:'d R.t -> unit -> 'e R.t *)
(*   (\**  Calculates correlations. *\) *)

(*   val lm : 'a R.t -> ?data:'b R.t -> ?subset:'c R.t -> ?weights:'d R.t -> *)
(*     ?na_action:'e R.t -> ?lm_method:'f R.t -> ?model:'g R.t -> ?x:'h R.t -> *)
(*     ?y:'i R.t -> ?qr:'j R.t -> ?singular_ok:'k R.t -> ?contrasts:'l R.t -> *)
(*     ?offset:'m R.t -> unit -> 'n R.t *)
(*   (\**  Makes a linear regression. *\) *)



(* end *)
