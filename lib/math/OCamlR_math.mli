(*********************************************************************************)
(*                OCaml-R                                                        *)
(*                                                                               *)
(*    Copyright (C) 2008-2010 Institut National de Recherche en                  *)
(*    Informatique et en Automatique. All rights reserved.                       *)
(*                                                                               *)
(*    Copyright (C) 2009-2010 Guillaume Yziquel. All rights reserved.            *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation; either version 3 of the         *)
(*    License, or  any later version.                                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public                  *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*             guillaume.yziquel@citycable.ch                                    *)
(*********************************************************************************)

(** Access to the R math library. *)

(** {2 Utilities} *)

val pow : float -> float -> float
(**  [pow x y] is the exponentiation of base [x] and power [y]. *)

val pow_di : float -> int -> float
(**  [pow x n] is the exponentiation of base [x] and integer power [n]. *)

(** {2 Random number generators} *)

val norm_rand : unit -> float
(**  Random variates from the standard normal distribution. *)

val unif_rand : unit -> float
(**  Random variates from the uniform distribution, using
  *  Marsaglia MultiCarry methodology for random number generation. *)

val exp_rand : unit -> float
(**  Random variates from the standard exponential distribution. *)

(** {2 Normal distribution} *)

val dnorm : ?mean:float -> ?sd:float -> ?log:bool -> float -> float
(** [dnorm ~mean:mu ~sd:sigma ~log:give_log x] computes the density
    of the normal distribution.
    @param mean Mean of the normal distribution.
    @param sd Standard deviation of the normal distribution.
    @param log If true, returns the logarithm of the density. *)

val pnorm :
  ?mean:float ->
  ?sd:float -> ?lower_tail:bool -> ?logp:bool -> float -> float

val qnorm :
  ?mean:float ->
  ?sd:float -> ?lower_tail:bool -> ?logp:bool -> float -> float

val rnorm : ?mean:float -> ?sd:float -> unit -> float

(** {2 Uniform distribution} *)

val dunif : ?min:float -> ?max:float -> ?log:bool -> float -> float

val punif :
  ?min:float ->
  ?max:float -> ?lower_tail:bool -> ?logp:bool -> float -> float

val qunif :
  ?min:float ->
  ?max:float -> ?lower_tail:bool -> ?logp:bool -> float -> float

val runif : ?min:float -> ?max:float -> unit -> float

(** {2 Gamma distribution} *)

val dgamma : ?scale:float -> ?log:bool -> shape:float -> float -> float

val pgamma :
  ?scale:float ->
  ?lower_tail:bool -> ?logp:bool -> shape:float -> float -> float

val qgamma :
  ?scale:float ->
  ?lower_tail:bool -> ?logp:bool -> shape:float -> float -> float

val rgamma : ?scale:float -> float -> float

val logspace_add : float -> float -> float
val logspace_sub : float -> float -> float

(** {2 Beta distribution} *)

val dbeta : ?ncp:float -> ?log:bool -> a:float -> b:float -> float -> float

val pbeta :
  ?ncp:float ->
  ?lower_tail:bool -> ?logp:bool -> a:float -> b:float -> float -> float

val qbeta :
  ?ncp:float ->
  ?lower_tail:bool -> ?logp:bool -> a:float -> b:float -> float -> float

val rbeta : a:float -> b:float -> float

(** {2 Lognormal distribution} *)

val dlnorm : ?meanlog:float -> ?sdlog:float -> ?log:bool -> float -> float

val plnorm :
  ?meanlog:float ->
  ?sdlog:float -> ?lower_tail:bool -> ?logp:bool -> float -> float

val qlnorm :
  ?meanlog:float ->
  ?sdlog:float -> ?lower_tail:bool -> ?logp:bool -> float -> float

val rlnorm : ?meanlog:float -> ?sdlog:float -> unit -> float

(** {2 Chi-squared distribution} *)

val dchisq : ?ncp:float -> ?log:bool -> df:float -> float -> float

val pchisq :
  ?ncp:float -> ?lower_tail:bool -> ?logp:bool -> df:float -> float -> float

val qchisq :
  ?ncp:float -> ?lower_tail:bool -> ?logp:bool -> df:float -> float -> float

val rchisq : ?ncp:float -> float -> float

(** {2 F distribution} *)

val df : ?ncp:float -> ?log:bool -> df1:float -> df2:float -> float -> float

val pf :
  ?ncp:float ->
  ?lower_tail:bool -> ?logp:bool -> df1:float -> df2:float -> float -> float

val qf :
  ?ncp:float ->
  ?lower_tail:bool -> ?logp:bool -> df1:float -> df2:float -> float -> float

val rf : df1:float -> df2:float -> float

(** {2 Student t distribution} *)

val dt : ?ncp:float -> ?log:bool -> df:float -> float -> float

val pt :
  ?ncp:float -> ?lower_tail:bool -> ?logp:bool -> df:float -> float -> float

val qt :
  ?ncp:float -> ?lower_tail:bool -> ?logp:bool -> df:float -> float -> float

val rt : float -> float

(** {2 Binomial distribution} *)

val dbinom : ?log:bool -> size:float -> prob:float -> float -> float

val pbinom :
  ?lower_tail:bool ->
  ?logp:bool -> size:float -> prob:float -> float -> float

val qbinom :
  ?lower_tail:bool ->
  ?logp:bool -> size:float -> prob:float -> float -> float

val rbinom : size:float -> prob:float -> float

(** {2 Cauchy distribution} *)

val dcauchy : ?location:float -> ?scale:float -> ?log:bool -> float -> float

val pcauchy :
  ?location:float ->
  ?scale:float -> ?lower_tail:bool -> ?logp:bool -> float -> float

val qcauchy :
  ?location:float ->
  ?scale:float -> ?lower_tail:bool -> ?logp:bool -> float -> float

val rcauchy : ?location:float -> ?scale:float -> unit -> float

(** {2 Exponential distribution} *)

val dexp : ?rate:float -> ?log:bool -> float -> float

val pexp : ?rate:float -> ?lower_tail:bool -> ?logp:bool -> float -> float

val qexp : ?rate:float -> ?lower_tail:bool -> ?logp:bool -> float -> float

val rexp : ?rate:float -> unit -> float

(** {2 Geometric distribution} *)

val dgeom : ?log:bool -> prob:float -> float -> float

val pgeom : ?lower_tail:bool -> ?logp:bool -> prob:float -> float -> float

val qgeom : ?lower_tail:bool -> ?logp:bool -> prob:float -> float -> float

val rgeom : float -> float

(** {2 Hypergeometric distribution} *)

val dhyper : ?log:bool -> m:float -> n:float -> k:float -> float -> float

val phyper :
  ?lower_tail:bool ->
  ?logp:bool -> m:float -> n:float -> k:float -> float -> float

val qhyper :
  ?lower_tail:bool ->
  ?logp:bool -> m:float -> n:float -> k:float -> float -> float

val rhyper : m:float -> n:float -> k:float -> float

(** {2 Negative binomial distribution} *)

val dnbinom : ?log:bool -> size:float -> prob:float -> float -> float

val pnbinom :
  ?lower_tail:bool ->
  ?logp:bool -> size:float -> prob:float -> float -> float

val qnbinom :
  ?lower_tail:bool ->
  ?logp:bool -> size:float -> prob:float -> float -> float

val rnbinom : size:float -> prob:float -> float

(** {2 Poisson distribution} *)

val dpois : ?log:bool -> lambda:float -> float -> float

val ppois : ?lower_tail:bool -> ?logp:bool -> lambda:float -> float -> float

val qpois : ?lower_tail:bool -> ?logp:bool -> lambda:float -> float -> float

val rpois : float -> float

(** {2 Weibull distribution} *)

val dweibull : ?scale:float -> ?log:bool -> shape:float -> float -> float

val pweibull :
  ?scale:float ->
  ?lower_tail:bool -> ?logp:bool -> shape:float -> float -> float

val qweibull :
  ?scale:float ->
  ?lower_tail:bool -> ?logp:bool -> shape:float -> float -> float

val rweibull : ?scale:float -> float -> float

(** {2 Logistic distribution} *)

val dlogis : ?location:float -> ?scale:float -> ?log:bool -> float -> float

val plogis :
  ?location:float ->
  ?scale:float -> ?lower_tail:bool -> ?logp:bool -> float -> float

val qlogis :
  ?location:float ->
  ?scale:float -> ?lower_tail:bool -> ?logp:bool -> float -> float

val rlogis : ?location:float -> ?scale:float -> unit -> float

(** {2 Studentized Range distribution} *)

val ptukey :
  ?nranges:float ->
  ?lower_tail:bool ->
  ?logp:bool -> nmeans:float -> df:float -> float -> float

val qtukey :
  ?nranges:float ->
  ?lower_tail:bool ->
  ?logp:bool -> nmeans:float -> df:float -> float -> float

(** {2 Wilcoxon Rank Sum distribution} *)

val dwilcox : ?log:bool -> m:float -> n:float -> float -> float

val pwilcox :
  ?lower_tail:bool -> ?logp:bool -> m:float -> n:float -> float -> float

val qwilcox :
  ?lower_tail:bool -> ?logp:bool -> m:float -> n:float -> float -> float

val rwilcox : m:float -> n:float -> float

(** {2 Wilcoxon Sign Rank distribution} *)

val dsignrank : ?log:bool -> n:float -> float -> float

val psignrank : ?lower_tail:bool -> ?logp:bool -> n:float -> float -> float

val qsignrank : ?lower_tail:bool -> ?logp:bool -> n:float -> float -> float

val rsignrank : float -> float


(** {2 Gamma and related functions} *)

val gammafn : float -> float
val lgammafn : float -> float
val psigamma : ?deriv:float -> float -> float
val digamma : float -> float
val trigamma : float -> float

val beta : a:float -> b:float -> float
val lbeta : a:float -> b:float -> float

val choose : n:float -> k:float -> float
val lchoose : n:float -> k:float -> float

(** {2 Bessel functions} *)

val bessel_i : ?expon_scaled:bool -> nu:float -> float -> float
val bessel_k : ?expon_scaled:bool -> nu:float -> float -> float
val bessel_j : x:float -> nu:float -> float
val bessel_y : x:float -> nu:float -> float

(** {2 General support functions} *)

val sign : float -> float
val ftrunc : float -> float
val fround : ?digits:int -> float -> float
val fsign : ?digits:int -> float -> float
val log1pmx : float -> float
val lgamma1p : float -> float
