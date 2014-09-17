(** Runtime R statistics library. *)

(** {5 Random number generation} *)

val rnorm : ?mean:float -> ?sd:float -> int -> float list
(** Random generation for the normal distribution. [mean] and [sd] default to [0.]
    and [1.] respectively. *)


(** {5 Testing} *)

(* class type fisher'test = object *)
(*   inherit list_ *)
(*   method p'value : float *)
(*   method conf'int : (float * float) option *)
(*   method estimate : float *)
(*   (\* method null'value : real ; *\) *)
(*   (\* method alternative : string ; *\) *)
(*   (\* method method_ : string ; *\) *)
(*   (\* method data'name : string *\) *)
(* end *)

(* val fisher'test : *)
(*   ?alternative:[`two_sided | `greater | `less] -> *)
(*   float list -> *)
(*   float list -> *)
(*   fisher'test *)

(* (\* val fisher'test_2x2 : *\) *)
(* (\*   ?alternative:[`two_sided | `greater | `less] -> *\) *)
(* (\*   ff:int -> ft:int -> tf:int -> tt:int -> unit -> fisher'test listing R.t *\) *)


(* val ks'test : *)
(*   ?alternative:[`two_sided | `greater | `less] -> *)
(*   float list -> float list -> *)
(*   < statistic : float R.t ; *)
(*     p'value : float R.t ; *)
(*     alternative : string R.t ; *)
(*     _method : string R.t ; *)
(*     data'name : string R.t > listing R.t *)

(* val p'adjust : *)
(*   ?method_ : [`holm | `hochberg | `hommel | `bonferroni | `BH | `BY | `fdr] -> *)
(*   float list -> float list *)




(* module Stub : sig *)

(*   (\** {5 Random number generation} *\) *)

(*   val rnorm : ?mean:float R.t -> ?sd:float R.t -> int R.t -> float list R.t *)


(*   (\** {5 Tests} *\) *)

(*   val fisher'test : *)
(*     ?alternative:string R.t -> *)
(*     float list R.t -> *)
(*     float list R.t -> *)
(*     < p'value : float R.t ; *)
(*       conf'int : float list R.t ; *)
(*       estimate : float R.t ; *)
(*       null'value : float R.t ; *)
(*       alternative : string R.t ; *)
(*       method_ : string R.t ; *)
(*       data'name : string R.t > listing R.t *)


(*   (\** {5 Misc} *\) *)

(*   val cor : 'a R.t -> ?y:'b R.t -> ?use:'c R.t -> ?cor_method:'d R.t -> unit -> 'e R.t *)
(*   (\**  Calculates correlations. *\) *)

(*   val lm : 'a R.t -> ?data:'b R.t -> ?subset:'c R.t -> ?weights:'d R.t -> *)
(*     ?na_action:'e R.t -> ?lm_method:'f R.t -> ?model:'g R.t -> ?x:'h R.t -> *)
(*     ?y:'i R.t -> ?qr:'j R.t -> ?singular_ok:'k R.t -> ?contrasts:'l R.t -> *)
(*     ?offset:'m R.t -> unit -> 'n R.t *)
(*   (\**  Makes a linear regression. *\) *)



(* end *)



