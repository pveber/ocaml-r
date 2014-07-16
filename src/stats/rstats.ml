open Rbase

let () = ignore (R.eval_string "require(stats, quietly=TRUE)")

module Stub = struct

  let cor = R.symbol "cor"

  let lm = R.symbol "lm"

  let stl = R.symbol "stl"

  let ks'test = R.symbol "ks.test"

  let fisher'test = R.symbol "fisher.test"

  let poisson_test = R.symbol "poisson.test"

  let shapiro_test = R.symbol "shapiro.test"

  let fitted = R.symbol "fitted"

  let p'adjust = R.symbol "p.adjust"

  let sSgompertz = R.symbol "SSgompertz"

  (* The log normal distribution. *)
  let dnorm = R.symbol "dnorm"
  let pnorm = R.symbol "pnorm"
  let qnorm = R.symbol "qnorm"
  let rnorm = R.symbol "rnorm"

  (* The log normal distribution. *)
  let dlnorm = R.symbol "dlnorm"
  let plnorm = R.symbol "plnorm"
  let qlnorm = R.symbol "qlnorm"
  let rlnorm = R.symbol "rlnorm"

end

let rnorm ?mean ?sd n =
  R.eval Stub.rnorm [
    R.arg R.int          n ;
    R.opt R.float "mean" mean ;
    R.opt R.float "sd"   sd ;
  ]

let cor x ?y ?use ?cor_method () =
  R.eval Stub.cor [
    R.arg (fun x -> x) x                   ;
    R.opt (fun x -> x) "y" y               ;
    R.opt (fun x -> x) "use" use           ;
    R.opt (fun x -> x) "method" cor_method ]

let lm formula ?data ?subset ?weights ?na_action ?lm_method ?model ?x ?y ?qr ?singular_ok ?contrasts ?offset () =
  R.eval Stub.lm [
    R.arg (fun x -> x)                formula     ;
    R.opt (fun x -> x) "data"         data        ;
    R.opt (fun x -> x) "subset"       subset      ;
    R.opt (fun x -> x) "weights"      weights     ;
    R.opt (fun x -> x) "na.action"    na_action   ;
    R.opt (fun x -> x) "method"       lm_method   ;
    R.opt (fun x -> x) "model"        model       ;
    R.opt (fun x -> x) "x"            x           ;
    R.opt (fun x -> x) "y"            y           ;
    R.opt (fun x -> x) "qr"           qr          ;
    R.opt (fun x -> x) "singular.ok"  singular_ok ;
    R.opt (fun x -> x) "contrasts"    contrasts   ;
    R.opt (fun x -> x) "offset"       offset      ]

let string_of_test_kind = function
| `two_sided -> "two.sided"
| `greater -> "greater"
| `less -> "less"

type fisher'test =
  < p'value : float R.t ;
    conf'int : float list R.t ;
    estimate : float R.t ;
    null'value : float R.t ;
    alternative : string R.t ;
    _method : string R.t ;
    data'name : string R.t >

let fisher'test ?alternative v v' =
  R.eval Stub.fisher'test [
    R.arg R.floats v ;
    R.arg R.floats v' ;
    R.opt (fun x -> R.string (string_of_test_kind x)) "alternative" alternative ;
  ]

let fisher'test_2x2 ?alternative ~ff ~ft ~tf ~tt () =
  let data = List.map float [ ff ; ft ; tf ; tt ] in
  R.eval Stub.fisher'test [
    R.arg (fun x -> matrix ~nrow:2 ~ncol:2 x) data ;
    R.opt (fun x -> R.string (string_of_test_kind x)) "alternative" alternative ;
  ]

let ks'test ?alternative v v' =
  R.eval Stub.ks'test [
    R.arg R.floats v ;
    R.arg R.floats v' ;
    R.opt (fun x -> R.string (string_of_test_kind x)) "alternative" alternative ;
  ]

let string_of_p'adjust_method = function
| `fdr -> "fdr"
| `holm -> "holm"
| `hochberg -> "hochberg"
| `hommel -> "hommel"
| `bonferroni -> "bonferroni"
| `BH -> "BH"
| `BY -> "BY"

let p'adjust ?method_ data =
  R.floats_of_t (
    R.eval Stub.p'adjust [
      R.arg R.floats data ;
      R.opt (fun x -> R.string (string_of_p'adjust_method x)) "method" method_
    ]
  )


















