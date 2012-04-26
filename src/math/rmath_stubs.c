/*********************************************************************************/
/*                OCaml-R                                                        */
/*                                                                               */
/*    Copyright (C) 2008-2010 Institut National de Recherche en                  */
/*    Informatique et en Automatique. All rights reserved.                       */
/*                                                                               */
/*    Copyright (C) 2009-2010 Guillaume Yziquel. All rights reserved.            */
/*                                                                               */
/*    This program is free software; you can redistribute it and/or modify       */
/*    it under the terms of the GNU General Public License as                    */
/*    published by the Free Software Foundation; either version 3 of the         */
/*    License, or  any later version.                                            */
/*                                                                               */
/*    This program is distributed in the hope that it will be useful,            */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               */
/*    GNU Library General Public License for more details.                       */
/*                                                                               */
/*    You should have received a copy of the GNU General Public                  */
/*    License along with this program; if not, write to the Free Software        */
/*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   */
/*    02111-1307  USA                                                            */
/*                                                                               */
/*    Contact: Maxence.Guesdon@inria.fr                                          */
/*             guillaume.yziquel@citycable.ch                                    */
/*********************************************************************************/

/* Access to the Rmath library */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#define MATHLIB_STANDALONE
#include <Rmath.h>
#include "wrappers.h"


ML_2(R_pow,Double_val,Double_val,caml_copy_double)
ML_2(R_pow_di,Double_val,Int_val,caml_copy_double)

ML_0(norm_rand,caml_copy_double)
ML_0(unif_rand,caml_copy_double)
ML_0(exp_rand,caml_copy_double)

ML_4(dnorm,Double_val,Double_val,Double_val,Bool_val,caml_copy_double)
ML_5(pnorm,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_5(qnorm,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_2(rnorm,Double_val,Double_val,caml_copy_double)

ML_4(dunif,Double_val,Double_val,Double_val,Bool_val,caml_copy_double)
ML_5(punif,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_5(qunif,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_2(runif,Double_val,Double_val,caml_copy_double)

ML_4(dgamma,Double_val,Double_val,Double_val,Bool_val,caml_copy_double)
ML_5(pgamma,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_5(qgamma,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_2(rgamma,Double_val,Double_val,caml_copy_double)

//ML_1(log1mpx,Double_val,caml_copy_double)
ML_1(lgamma1p,Double_val,caml_copy_double)
ML_2(logspace_add,Double_val,Double_val,caml_copy_double)
ML_2(logspace_sub,Double_val,Double_val,caml_copy_double)

ML_4(dbeta,Double_val,Double_val,Double_val,Bool_val,caml_copy_double)
ML_5(pbeta,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_5(qbeta,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_2(rbeta,Double_val,Double_val,caml_copy_double)

ML_5(dnbeta,Double_val,Double_val,Double_val,Double_val,Bool_val,caml_copy_double)
ML_6(pnbeta,Double_val,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_bc6(ml_pnbeta)
ML_6(qnbeta,Double_val,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_bc6(ml_qnbeta)

ML_4(dlnorm,Double_val,Double_val,Double_val,Bool_val,caml_copy_double)
ML_5(plnorm,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_5(qlnorm,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_2(rlnorm,Double_val,Double_val,caml_copy_double)

ML_3(dchisq,Double_val,Double_val,Bool_val,caml_copy_double)
ML_4(pchisq,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_4(qchisq,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_1(rchisq,Double_val,caml_copy_double)

ML_4(dnchisq,Double_val,Double_val,Double_val,Bool_val,caml_copy_double)
ML_5(pnchisq,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_5(qnchisq,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_2(rnchisq,Double_val,Double_val,caml_copy_double)

ML_4(df,Double_val,Double_val,Double_val,Bool_val,caml_copy_double)
ML_5(pf,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_5(qf,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_2(rf,Double_val,Double_val,caml_copy_double)

ML_5(dnf,Double_val,Double_val,Double_val,Double_val,Bool_val,caml_copy_double)
ML_6(pnf,Double_val,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_bc6(ml_pnf)
ML_6(qnf,Double_val,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_bc6(ml_qnf)

ML_3(dt,Double_val,Double_val,Bool_val,caml_copy_double)
ML_4(pt,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_4(qt,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_1(rt,Double_val,caml_copy_double)

ML_4(dnt,Double_val,Double_val,Double_val,Bool_val,caml_copy_double)
ML_5(pnt,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_5(qnt,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)

ML_4(dbinom,Double_val,Double_val,Double_val,Bool_val,caml_copy_double)
ML_5(pbinom,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_5(qbinom,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_2(rbinom,Double_val,Double_val,caml_copy_double)

ML_4(dcauchy,Double_val,Double_val,Double_val,Bool_val,caml_copy_double)
ML_5(pcauchy,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_5(qcauchy,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_2(rcauchy,Double_val,Double_val,caml_copy_double)

ML_3(dexp,Double_val,Double_val,Bool_val,caml_copy_double)
ML_4(pexp,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_4(qexp,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_1(rexp,Double_val,caml_copy_double)

ML_3(dgeom,Double_val,Double_val,Bool_val,caml_copy_double)
ML_4(pgeom,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_4(qgeom,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_1(rgeom,Double_val,caml_copy_double)

ML_5(dhyper,Double_val,Double_val,Double_val,Double_val,Bool_val,caml_copy_double)
ML_6(phyper,Double_val,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_bc6(ml_phyper)
ML_6(qhyper,Double_val,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_bc6(ml_qhyper)
ML_3(rhyper,Double_val,Double_val,Double_val,caml_copy_double)

ML_4(dnbinom,Double_val,Double_val,Double_val,Bool_val,caml_copy_double)
ML_5(pnbinom,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_5(qnbinom,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_2(rnbinom,Double_val,Double_val,caml_copy_double)

ML_3(dpois,Double_val,Double_val,Bool_val,caml_copy_double)
ML_4(ppois,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_4(qpois,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_1(rpois,Double_val,caml_copy_double)

ML_4(dweibull,Double_val,Double_val,Double_val,Bool_val,caml_copy_double)
ML_5(pweibull,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_5(qweibull,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_2(rweibull,Double_val,Double_val,caml_copy_double)

ML_4(dlogis,Double_val,Double_val,Double_val,Bool_val,caml_copy_double)
ML_5(plogis,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_5(qlogis,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_2(rlogis,Double_val,Double_val,caml_copy_double)

ML_6(ptukey,Double_val,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double);
ML_bc6(ml_ptukey);
ML_6(qtukey,Double_val,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double);
ML_bc6(ml_qtukey);
ML_4(dwilcox,Double_val,Double_val,Double_val,Bool_val,caml_copy_double)
ML_5(pwilcox,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_5(qwilcox,Double_val,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_2(rwilcox,Double_val,Double_val,caml_copy_double)

ML_3(dsignrank,Double_val,Double_val,Bool_val,caml_copy_double)
ML_4(psignrank,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_4(qsignrank,Double_val,Double_val,Bool_val,Bool_val,caml_copy_double)
ML_1(rsignrank,Double_val,caml_copy_double)

ML_1(gammafn,Double_val,caml_copy_double)
ML_1(lgammafn,Double_val,caml_copy_double)

ML_2(psigamma,Double_val,Double_val,caml_copy_double)

ML_1(digamma,Double_val,caml_copy_double)
ML_1(trigamma,Double_val,caml_copy_double)

ML_2(beta,Double_val,Double_val,caml_copy_double)
ML_2(lbeta,Double_val,Double_val,caml_copy_double)

ML_2(choose,Double_val,Double_val,caml_copy_double)
ML_2(lchoose,Double_val,Double_val,caml_copy_double)

ML_3(bessel_i,Double_val,Double_val,Double_val,caml_copy_double)
ML_3(bessel_k,Double_val,Double_val,Double_val,caml_copy_double)
ML_2(bessel_j,Double_val,Double_val,caml_copy_double)
ML_2(bessel_y,Double_val,Double_val,caml_copy_double)

ML_1(sign,Double_val,caml_copy_double)
ML_1(ftrunc,Double_val,caml_copy_double)
ML_2(fround,Double_val,Double_val,caml_copy_double)
ML_2(fsign,Double_val,Double_val,caml_copy_double)

ML_1(log1pmx,Double_val,caml_copy_double)

