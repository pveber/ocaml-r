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

(** Bindings to the Rmath library. *)

external pow : float -> float -> float = "ml_R_pow"
external pow_di : float -> int -> float = "ml_R_pow_di"
external norm_rand : unit -> float = "ml_norm_rand"
external unif_rand : unit -> float = "ml_unif_rand"
external exp_rand : unit -> float = "ml_exp_rand"

external r_dnorm : x: float -> mean: float -> sd: float -> log: bool -> float = "ml_dnorm";;
let dnorm ?(mean=0.0) ?(sd=1.0) ?(log=false) x = r_dnorm ~x ~mean ~sd ~log;;

external r_pnorm : q:float -> mean:float -> sd:float ->
  lower_tail:bool -> logp:bool -> float = "ml_pnorm";;
let pnorm ?(mean=0.0) ?(sd=1.0) ?(lower_tail=true) ?(logp=false) q =
  r_pnorm ~q ~mean ~sd ~lower_tail ~logp

external r_qnorm : p:float -> mean:float -> sd:float ->
  lower_tail:bool -> logp:bool -> float = "ml_qnorm";;
let qnorm ?(mean=0.0) ?(sd=1.0) ?(lower_tail=true) ?(logp=false) p =
  r_qnorm ~p ~mean ~sd ~lower_tail ~logp;;

external r_rnorm : mean:float -> sd:float -> float = "ml_rnorm";;
let rnorm ?(mean=0.0) ?(sd=1.0) () = r_rnorm ~mean ~sd;;


external r_dunif : x: float -> min: float -> max: float -> log: bool -> float = "ml_dunif";;
let dunif ?(min=0.0) ?(max=1.0) ?(log=false) x = r_dunif ~x ~min ~max ~log;;

external r_punif : q:float -> min:float -> max:float ->
  lower_tail:bool -> logp:bool -> float = "ml_punif";;
let punif ?(min=0.0) ?(max=1.0) ?(lower_tail=true) ?(logp=false) q =
  r_punif ~q ~min ~max ~lower_tail ~logp;;

external r_qunif : p:float -> min:float -> max:float ->
  lower_tail:bool -> logp:bool -> float = "ml_qunif";;
let qunif ?(min=0.0) ?(max=1.0) ?(lower_tail=true) ?(logp=false) p =
  r_qunif ~p ~min ~max ~lower_tail ~logp;;

external r_runif : min:float -> max:float -> float = "ml_runif";;
let runif ?(min=0.0) ?(max=1.0) () = r_runif ~min ~max;;


external r_dgamma : x: float -> shape: float -> scale: float -> log: bool -> float = "ml_dgamma";;
let dgamma ?(scale=1.0) ?(log=false) ~shape x = r_dgamma ~x ~shape ~scale ~log;;

external r_pgamma : q:float -> shape:float -> scale:float ->
  lower_tail:bool -> logp:bool -> float = "ml_pgamma";;
let pgamma ?(scale=1.0) ?(lower_tail=true) ?(logp=false) ~shape q =
  r_pgamma ~q ~shape ~scale ~lower_tail ~logp;;

external r_qgamma : p:float -> shape:float -> scale:float ->
  lower_tail:bool -> logp:bool -> float = "ml_qgamma";;
let qgamma ?(scale=1.0) ?(lower_tail=true) ?(logp=false) ~shape p =
  r_qgamma ~p ~shape ~scale ~lower_tail ~logp;;

external r_rgamma : shape:float -> scale:float -> float = "ml_rgamma";;
let rgamma ?(scale=1.0) shape = r_rgamma ~shape ~scale;;

(*    external log1mpx : float -> float = "ml_log1mpx";;*)
external logspace_add : float -> float -> float = "ml_logspace_add";;
external logspace_sub : float -> float -> float = "ml_logspace_sub";;


external r_dbeta : x: float -> shape1: float -> shape2: float -> log: bool -> float = "ml_dbeta";;
external r_dnbeta : x: float -> shape1: float -> shape2: float -> ncp: float ->
  log: bool -> float = "ml_dnbeta";;
let dbeta ?ncp ?(log=false) ~a ~b x =
  match ncp with
    None -> r_dbeta ~x ~shape1:a ~shape2:b ~log
  | Some ncp -> r_dnbeta ~x ~shape1:a ~shape2:b ~ncp ~log
;;

external r_pbeta : q:float -> shape1:float -> shape2:float ->
  lower_tail:bool -> logp:bool -> float = "ml_pbeta";;
external r_pnbeta : q:float -> shape1:float -> shape2:float -> ncp:float ->
  lower_tail:bool -> logp:bool -> float = "ml_pnbeta_bc" "ml_pnbeta";;
let pbeta ?ncp ?(lower_tail=true) ?(logp=false) ~a ~b q =
  match ncp with
    None -> r_pbeta ~q ~shape1: a ~shape2: b ~lower_tail ~logp
  | Some ncp -> r_pnbeta ~q ~shape1: a ~shape2: b ~ncp ~lower_tail ~logp
;;

external r_qbeta : p:float -> shape1:float -> shape2:float ->
  lower_tail:bool -> logp:bool -> float = "ml_qbeta";;
external r_qnbeta : p:float -> shape1:float -> shape2:float -> ncp:float ->
  lower_tail:bool -> logp:bool -> float = "ml_qnbeta_bc" "ml_qnbeta";;
let qbeta ?ncp ?(lower_tail=true) ?(logp=false) ~a ~b p =
  match ncp with
    None -> r_qbeta ~p ~shape1: a ~shape2: b ~lower_tail ~logp
  | Some ncp -> r_qnbeta ~p ~shape1: a ~shape2: b ~ncp ~lower_tail ~logp
;;

external r_rbeta : shape1:float -> shape2:float -> float = "ml_rbeta";;
let rbeta ~a ~b = r_rbeta ~shape1:a ~shape2:b;;


external r_dlnorm : x: float -> meanlog: float -> sdlog: float -> log: bool -> float = "ml_dlnorm";;
let dlnorm ?(meanlog=0.0) ?(sdlog=1.0) ?(log=false) x = r_dlnorm ~x ~meanlog ~sdlog ~log;;

external r_plnorm : q:float -> meanlog:float -> sdlog:float ->
  lower_tail:bool -> logp:bool -> float = "ml_plnorm";;
let plnorm ?(meanlog=0.0) ?(sdlog=1.0) ?(lower_tail=true) ?(logp=false) q =
  r_plnorm ~q ~meanlog ~sdlog ~lower_tail ~logp;;

external r_qlnorm : p:float -> meanlog:float -> sdlog:float ->
  lower_tail:bool -> logp:bool -> float = "ml_qlnorm";;
let qlnorm ?(meanlog=0.0) ?(sdlog=1.0) ?(lower_tail=true) ?(logp=false) p =
  r_qlnorm ~p ~meanlog ~sdlog ~lower_tail ~logp;;

external r_rlnorm : meanlog:float -> sdlog:float -> float = "ml_rlnorm";;
let rlnorm ?(meanlog=0.0) ?(sdlog=1.0) () = r_rlnorm ~meanlog ~sdlog;;


external r_dchisq : x: float -> df: float -> log: bool -> float = "ml_dchisq";;
external r_dnchisq : x: float -> df: float -> ncp: float -> log: bool -> float = "ml_dnchisq";;
let dchisq ?ncp ?(log=false) ~df x =
  match ncp with
    None -> r_dchisq ~x ~df ~log
  | Some ncp -> r_dnchisq ~x ~df ~ncp ~log
;;

external r_pchisq : q:float -> df:float -> lower_tail:bool -> logp:bool -> float = "ml_pchisq";;
external r_pnchisq : q:float -> df:float -> ncp: float ->
      lower_tail:bool -> logp:bool -> float = "ml_pnchisq";;
let pchisq ?ncp ?(lower_tail=true) ?(logp=false) ~df q =
  match  ncp with
    None -> r_pchisq ~q ~df ~lower_tail ~logp
  | Some ncp -> r_pnchisq ~q ~df ~ncp ~lower_tail ~logp
;;

external r_qchisq : p:float -> df:float -> lower_tail:bool -> logp:bool -> float = "ml_qchisq";;
external r_qnchisq : p:float -> df:float -> ncp: float ->
      lower_tail:bool -> logp:bool -> float = "ml_qnchisq";;
let qchisq ?ncp ?(lower_tail=true) ?(logp=false) ~df p =
  match ncp with
    None -> r_qchisq ~p ~df ~lower_tail ~logp
  | Some ncp -> r_qnchisq ~p ~df ~ncp ~lower_tail ~logp;;

external r_rchisq : df:float -> float = "ml_rchisq";;
external r_rnchisq : df:float -> ncp:float -> float = "ml_rnchisq";;
let rchisq ?ncp df =
  match ncp with
    None -> r_rchisq ~df
  | Some ncp -> r_rnchisq ~df ~ncp
;;

external r_df : x: float -> df1: float -> df2: float -> log: bool -> float = "ml_df";;
external r_dnf : x: float -> df1: float -> df2: float -> ncp: float -> log: bool -> float = "ml_dnf";;
let df ?ncp ?(log=false) ~df1 ~df2 x =
  match ncp with
    None -> r_df ~x ~df1 ~df2 ~log
  | Some ncp -> r_dnf ~x ~df1 ~df2 ~ncp ~log
;;

external r_pf : q:float -> df1:float -> df2:float -> lower_tail:bool -> logp:bool -> float = "ml_pnf";;
external r_pnf : q:float -> df1:float -> df2:float -> ncp: float ->
      lower_tail:bool -> logp:bool -> float = "ml_pnf_bc" "ml_pnf";;
let pf ?ncp ?(lower_tail=true) ?(logp=false) ~df1 ~df2 q =
  match  ncp with
    None -> r_pf ~q ~df1 ~df2 ~lower_tail ~logp
  | Some ncp -> r_pnf ~q ~df1 ~df2 ~ncp ~lower_tail ~logp
;;

external r_qf : p:float -> df1:float -> df2:float -> lower_tail:bool -> logp:bool -> float = "ml_qf";;
external r_qnf : p:float -> df1:float -> df2:float -> ncp: float ->
      lower_tail:bool -> logp:bool -> float = "ml_qnf_bc" "ml_qnf";;
let qf ?ncp ?(lower_tail=true) ?(logp=false) ~df1 ~df2 p =
  match ncp with
    None -> r_qf ~p ~df1 ~df2 ~lower_tail ~logp
  | Some ncp -> r_qnf ~p ~df1 ~df2 ~ncp ~lower_tail ~logp;;

external rf : df1:float -> df2:float -> float = "ml_rf";;


external r_dt : x: float -> df: float -> log: bool -> float = "ml_dt";;
external r_dnt : x: float -> df: float -> ncp: float -> log: bool -> float = "ml_dnt";;
let dt ?ncp ?(log=false) ~df x =
  match ncp with
    None -> r_dt ~x ~df ~log
  | Some ncp -> r_dnt ~x ~df ~ncp ~log
;;

external r_pt : q:float -> df:float -> lower_tail:bool -> logp:bool -> float = "ml_pt";;
external r_pnt : q:float -> df:float -> ncp: float ->
      lower_tail:bool -> logp:bool -> float = "ml_pnt";;
let pt ?ncp ?(lower_tail=true) ?(logp=false) ~df q =
  match  ncp with
    None -> r_pt ~q ~df ~lower_tail ~logp
  | Some ncp -> r_pnt ~q ~df ~ncp ~lower_tail ~logp
;;

external r_qt : p:float -> df:float -> lower_tail:bool -> logp:bool -> float = "ml_qt";;
external r_qnt : p:float -> df:float -> ncp: float ->
      lower_tail:bool -> logp:bool -> float = "ml_qnt";;
let qt ?ncp ?(lower_tail=true) ?(logp=false) ~df p =
  match ncp with
    None -> r_qt ~p ~df ~lower_tail ~logp
  | Some ncp -> r_qnt ~p ~df ~ncp ~lower_tail ~logp;;

external rt : float -> float = "ml_rt";;

external r_dbinom : x: float -> size: float -> prob: float -> log: bool -> float = "ml_dbinom";;
let dbinom ?(log=false) ~size ~prob x = r_dbinom ~x ~size ~prob ~log;;

external r_pbinom : q:float -> size:float -> prob:float ->
  lower_tail:bool -> logp:bool -> float = "ml_pbinom";;
let pbinom ?(lower_tail=true) ?(logp=false) ~size ~prob q =
  r_pbinom ~q ~size ~prob ~lower_tail ~logp;;

external r_qbinom : p:float -> size:float -> prob:float ->
  lower_tail:bool -> logp:bool -> float = "ml_qbinom";;
let qbinom ?(lower_tail=true) ?(logp=false) ~size ~prob p =
  r_qbinom ~p ~size ~prob ~lower_tail ~logp;;

external rbinom : size:float -> prob:float -> float = "ml_rbinom";;


external r_dcauchy : x: float -> location: float -> scale: float -> log: bool -> float = "ml_dcauchy";;
let dcauchy ?(location=0.0) ?(scale=1.0) ?(log=false) x = r_dcauchy ~x ~location ~scale ~log;;

external r_pcauchy : q:float -> location:float -> scale:float ->
  lower_tail:bool -> logp:bool -> float = "ml_pcauchy";;
let pcauchy ?(location=0.0) ?(scale=1.0) ?(lower_tail=true) ?(logp=false) q =
  r_pcauchy ~q ~location ~scale ~lower_tail ~logp

external r_qcauchy : p:float -> location:float -> scale:float ->
  lower_tail:bool -> logp:bool -> float = "ml_qcauchy";;
let qcauchy ?(location=0.0) ?(scale=1.0) ?(lower_tail=true) ?(logp=false) p =
  r_qcauchy ~p ~location ~scale ~lower_tail ~logp;;

external r_rcauchy : location:float -> scale:float -> float = "ml_rcauchy";;
let rcauchy ?(location=0.0) ?(scale=1.0) () = r_rcauchy ~location ~scale;;


external r_dexp : x: float -> rate: float -> log: bool -> float = "ml_dexp";;
let dexp ?(rate=1.0) ?(log=false) x = r_dexp ~x ~rate ~log;;

external r_pexp : q:float -> rate:float ->
  lower_tail:bool -> logp:bool -> float = "ml_pexp";;
let pexp ?(rate=1.0) ?(lower_tail=true) ?(logp=false) q =
  r_pexp ~q ~rate ~lower_tail ~logp

external r_qexp : p:float -> rate:float ->
  lower_tail:bool -> logp:bool -> float = "ml_qexp";;
let qexp ?(rate=1.0) ?(lower_tail=true) ?(logp=false) p =
  r_qexp ~p ~rate ~lower_tail ~logp;;

external r_rexp : rate:float -> float = "ml_rexp";;
let rexp ?(rate=1.0) () = r_rexp ~rate;;


external r_dgeom : x: float -> prob: float -> log: bool -> float = "ml_dgeom";;
let dgeom ?(log=false) ~prob x = r_dgeom ~x ~prob ~log;;

external r_pgeom : q:float -> prob:float ->
  lower_tail:bool -> logp:bool -> float = "ml_pgeom";;
let pgeom ?(lower_tail=true) ?(logp=false) ~prob q =
  r_pgeom ~q ~prob ~lower_tail ~logp;;

external r_qgeom : p:float -> prob:float ->
  lower_tail:bool -> logp:bool -> float = "ml_qgeom";;
let qgeom ?(lower_tail=true) ?(logp=false) ~prob p =
  r_qgeom ~p ~prob ~lower_tail ~logp;;

external rgeom : float -> float = "ml_rgeom";;


external r_dhyper : x: float -> m:float -> n:float -> k:float -> log: bool -> float = "ml_dhyper";;
let dhyper ?(log=false) ~m ~n ~k x = r_dhyper ~x ~m ~n ~k ~log;;

external r_phyper : q:float -> m:float -> n:float -> k:float ->
  lower_tail:bool -> logp:bool -> float = "ml_phyper_bc" "ml_phyper";;
let phyper ?(lower_tail=true) ?(logp=false) ~m ~n ~k q =
  r_phyper ~q ~m ~n ~k ~lower_tail ~logp;;

external r_qhyper : p:float -> m:float -> n:float -> k:float ->
  lower_tail:bool -> logp:bool -> float = "ml_qhyper_bc" "ml_qhyper";;
let qhyper ?(lower_tail=true) ?(logp=false) ~m ~n ~k p =
  r_qhyper ~p ~m ~n ~k ~lower_tail ~logp;;

external rhyper : m:float -> n:float -> k:float -> float = "ml_rhyper";;


external r_dnbinom : x: float -> size: float -> prob: float -> log: bool -> float = "ml_dnbinom";;
let dnbinom ?(log=false) ~size ~prob x = r_dnbinom ~x ~size ~prob ~log;;

external r_pnbinom : q:float -> size:float -> prob:float ->
  lower_tail:bool -> logp:bool -> float = "ml_pnbinom";;
let pnbinom ?(lower_tail=true) ?(logp=false) ~size ~prob q =
  r_pnbinom ~q ~size ~prob ~lower_tail ~logp;;

external r_qnbinom : p:float -> size:float -> prob:float ->
  lower_tail:bool -> logp:bool -> float = "ml_qnbinom";;
let qnbinom ?(lower_tail=true) ?(logp=false) ~size ~prob p =
  r_qnbinom ~p ~size ~prob ~lower_tail ~logp;;

external rnbinom : size:float -> prob:float -> float = "ml_rnbinom";;


external r_dpois : x: float -> lambda: float -> log: bool -> float = "ml_dpois";;
let dpois ?(log=false) ~lambda x = r_dpois ~x ~lambda ~log;;

external r_ppois : q:float -> lambda:float ->
  lower_tail:bool -> logp:bool -> float = "ml_ppois";;
let ppois ?(lower_tail=true) ?(logp=false) ~lambda q =
  r_ppois ~q ~lambda ~lower_tail ~logp

external r_qpois : p:float -> lambda:float ->
  lower_tail:bool -> logp:bool -> float = "ml_qpois";;
let qpois ?(lower_tail=true) ?(logp=false) ~lambda p =
  r_qpois ~p ~lambda ~lower_tail ~logp;;

external r_rpois : lambda:float -> float = "ml_rpois";;
let rpois lambda = r_rpois ~lambda;;


external r_dweibull : x: float -> shape: float -> scale: float -> log: bool -> float = "ml_dweibull";;
let dweibull ?(scale=1.0) ?(log=false) ~shape x = r_dweibull ~x ~shape ~scale ~log;;

external r_pweibull : q:float -> shape:float -> scale:float ->
  lower_tail:bool -> logp:bool -> float = "ml_pweibull";;
let pweibull ?(scale=1.0) ?(lower_tail=true) ?(logp=false) ~shape q =
  r_pweibull ~q ~shape ~scale ~lower_tail ~logp;;

external r_qweibull : p:float -> shape:float -> scale:float ->
  lower_tail:bool -> logp:bool -> float = "ml_qweibull";;
let qweibull ?(scale=1.0) ?(lower_tail=true) ?(logp=false) ~shape p =
  r_qweibull ~p ~shape ~scale ~lower_tail ~logp;;

external r_rweibull : shape:float -> scale:float -> float = "ml_rweibull";;
let rweibull ?(scale=1.0) shape = r_rweibull ~shape ~scale;;


external r_dlogis : x: float -> location: float -> scale: float -> log: bool -> float = "ml_dlogis";;
let dlogis ?(location=0.0) ?(scale=1.0) ?(log=false) x = r_dlogis ~x ~location ~scale ~log;;

external r_plogis : q:float -> location:float -> scale:float ->
  lower_tail:bool -> logp:bool -> float = "ml_plogis";;
let plogis ?(location=0.0) ?(scale=1.0) ?(lower_tail=true) ?(logp=false) q =
  r_plogis ~q ~location ~scale ~lower_tail ~logp

external r_qlogis : p:float -> location:float -> scale:float ->
  lower_tail:bool -> logp:bool -> float = "ml_qlogis";;
let qlogis ?(location=0.0) ?(scale=1.0) ?(lower_tail=true) ?(logp=false) p =
  r_qlogis ~p ~location ~scale ~lower_tail ~logp;;

external r_rlogis : location:float -> scale:float -> float = "ml_rlogis";;
let rlogis ?(location=0.0) ?(scale=1.0) () = r_rlogis ~location ~scale;;


external r_ptukey : q:float -> nmeans:float -> df:float -> nranges:float ->
  lower_tail:bool -> logp:bool -> float = "ml_ptukey_bc" "ml_ptukey";;
let ptukey ?(nranges=1.0) ?(lower_tail=true) ?(logp=false) ~nmeans ~df q =
  r_ptukey ~q ~nmeans ~df ~nranges ~lower_tail ~logp;;

external r_qtukey : p:float -> nmeans:float -> df:float -> nranges:float ->
  lower_tail:bool -> logp:bool -> float = "ml_qtukey_bc" "ml_qtukey";;
let qtukey ?(nranges=1.0) ?(lower_tail=true) ?(logp=false) ~nmeans ~df p =
  r_qtukey ~p ~nmeans ~df ~nranges ~lower_tail ~logp;;


external r_dwilcox : x: float -> m:float -> n:float -> log: bool -> float = "ml_dwilcox";;
let dwilcox ?(log=false) ~m ~n x = r_dwilcox ~x ~m ~n ~log;;

external r_pwilcox : q:float -> m:float -> n:float ->
  lower_tail:bool -> logp:bool -> float = "ml_pwilcox";;
let pwilcox ?(lower_tail=true) ?(logp=false) ~m ~n q =
  r_pwilcox ~q ~m ~n ~lower_tail ~logp;;

external r_qwilcox : p:float -> m:float -> n:float ->
  lower_tail:bool -> logp:bool -> float = "ml_qwilcox";;
let qwilcox ?(lower_tail=true) ?(logp=false) ~m ~n p =
  r_qwilcox ~p ~m ~n ~lower_tail ~logp;;

external rwilcox : m:float -> n:float -> float = "ml_rwilcox";;

external r_dsignrank : x: float -> n: float -> log: bool -> float = "ml_dsignrank";;
let dsignrank ?(log=false) ~n x = r_dsignrank ~x ~n ~log;;

external r_psignrank : q:float -> n:float ->
  lower_tail:bool -> logp:bool -> float = "ml_psignrank";;
let psignrank ?(lower_tail=true) ?(logp=false) ~n q =
  r_psignrank ~q ~n ~lower_tail ~logp

external r_qsignrank : p:float -> n:float ->
  lower_tail:bool -> logp:bool -> float = "ml_qsignrank";;
let qsignrank ?(lower_tail=true) ?(logp=false) ~n p =
  r_qsignrank ~p ~n ~lower_tail ~logp;;

external r_rsignrank : n:float -> float = "ml_rsignrank";;
let rsignrank n = r_rsignrank ~n;;


external gammafn : float -> float = "ml_gammafn";;
external lgammafn : float -> float = "ml_lgammafn";;
(*void    dpsifn(double, int, int, int, double*, int*, int* );*)

external psigamma : x:float -> deriv: float -> float = "ml_psigamma";;
let psigamma ?(deriv=0.0) x = psigamma ~x ~deriv;;

external digamma : float -> float = "ml_digamma";;
external trigamma : float -> float = "ml_trigamma";;

external beta: a:float -> b:float -> float = "ml_beta";;
external lbeta: a:float -> b:float -> float = "ml_lbeta";;

external choose: n:float -> k:float -> float = "ml_choose";;
external lchoose: n:float -> k:float -> float = "ml_lchoose";;

external r_bessel_i: x:float -> nu:float -> expon_scaled:float -> float = "ml_bessel_i";;
let bessel_i ?(expon_scaled=true) ~nu x =
  let expon_scaled = if expon_scaled then 1.0 else 0.0 in
  r_bessel_i ~x ~nu ~expon_scaled
;;

external r_bessel_k : x:float -> nu:float -> expon_scaled:float -> float = "ml_bessel_k";;
let bessel_k ?(expon_scaled=true) ~nu x =
  let expon_scaled = if expon_scaled then 1.0 else 0.0 in
  r_bessel_k ~x ~nu ~expon_scaled
;;

external bessel_j : x:float -> nu:float -> float = "ml_bessel_j";;
external bessel_y : x:float -> nu:float -> float = "ml_bessel_y";;

external sign: float -> float = "ml_sign";;
external ftrunc: float -> float = "ml_ftrunc";;

external r_round: x:float -> digits:float -> float = "ml_fround";;
let fround ?(digits=0) x = r_round ~x ~digits: (float digits);;

external r_fsign: x:float -> digits:float -> float = "ml_fsign";;
let fsign ?(digits=0) x = r_fsign ~x ~digits: (float digits);;

external log1pmx: float -> float = "ml_log1pmx";;
external lgamma1p: float -> float = "ml_lgamma1p";;



