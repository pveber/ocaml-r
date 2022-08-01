(** Runtime R statistics library. *)

open OCamlR_base

val formula : string -> Formula.t

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

val rnorm : ?mean:float -> ?sd:float -> int -> Numeric.t
(** Random generation for the normal distribution. [mean] and [sd] default to [0.]
    and [1.] respectively. *)


(** {2 Tests} *)

(** Common interface for test results *)
module type Test = sig
  include module type of List_
  val p'value : t -> float
  val _method_ : t -> string
  val data'name : t -> string
  val alternative : t -> string
end

(** Student's T test *)
module T'test : sig
  include Test
  val conf'int : t -> (float * float) option
  val estimate : t -> float
  val null'value : t -> float

  val one_sample :
    ?alternative:[`two_sided | `greater | `less] ->
    Numeric.t ->
    t
end

(** Fisher's exact test for independence *)
module Fisher'test : sig
  include Test
  val conf'int : t -> (float * float) option
  val estimate : t -> float
  val null'value : t -> float

  val logicals :
    ?alternative:[`two_sided | `greater | `less] ->
    Logical.t ->
    Logical.t ->
    t
end

(** Chi-squared test for independence *)
module Chisq'test : sig
  include Test
  val statistic : t -> float

  val contingency_table :
    ?correct:bool ->
    ?simulate'p'value:bool ->
    ?b:int ->
    Integer.Matrix.t ->
    t
end

(** Kolmogorov-Smirnov test *)
module Ks'test : sig
  include Test
  val statistic : t -> float
  val make :
    ?alternative:[`two_sided | `greater | `less] ->
    Numeric.t ->
    Numeric.t ->
    t
end

val p'adjust :
  ?method_ : [`holm | `hochberg | `hommel | `bonferroni | `BH | `BY | `fdr] ->
  Numeric.t ->
  Numeric.t
