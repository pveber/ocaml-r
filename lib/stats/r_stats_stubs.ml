let () = ignore (R.eval_string "require(stats, quietly=TRUE)")

let id x = x

module Symbol = struct
  let rnorm = R.symbol "rnorm"
  let dnorm = R.symbol "dnorm"
  let pnorm = R.symbol "pnorm"
  let qnorm = R.symbol "qnorm"

  (* The log normal distribution. *)
  let dlnorm = R.symbol "dlnorm"
  let plnorm = R.symbol "plnorm"
  let qlnorm = R.symbol "qlnorm"
  let rlnorm = R.symbol "rlnorm"

  let cor = R.symbol "cor"
  let lm = R.symbol "lm"
  let stl = R.symbol "stl"

  let fisher'test = R.symbol "fisher.test"
  let poisson_test = R.symbol "poisson.test"
  let shapiro_test = R.symbol "shapiro.test"
  let fitted = R.symbol "fitted"
  let sSgompertz = R.symbol "SSgompertz"
end

let rnorm ?mean ?sd n =
  R.eval Symbol.rnorm [
    R.arg id n ;
    R.opt id "mean" mean ;
    R.opt id "sd" sd
  ]

let fisher'test ?alternative v v' =
  R.eval Symbol.fisher'test [
    R.arg id v ;
    R.arg id v' ;
    R.opt id "alternative" alternative ;
  ]

let cor x ?y ?use ?cor_method () =
  R.eval Symbol.cor [
    R.arg (fun x -> x) x                   ;
    R.opt (fun x -> x) "y" y               ;
    R.opt (fun x -> x) "use" use           ;
    R.opt (fun x -> x) "method" cor_method ]

let lm formula ?data ?subset ?weights ?na_action ?lm_method ?model ?x ?y ?qr ?singular_ok ?contrasts ?offset () =
  R.eval Symbol.lm [
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

(* let fisher'test_2x2 ?alternative ~ff ~ft ~tf ~tt () = *)
(*   let data = List.map float [ ff ; ft ; tf ; tt ] in *)
(*   R.eval Stub.fisher'test [ *)
(*     R.arg (fun x -> matrix ~nrow:2 ~ncol:2 x) data ; *)
(*     R.opt (fun x -> R.string (string_of_test_kind x)) "alternative" alternative ; *)
(*   ] *)

