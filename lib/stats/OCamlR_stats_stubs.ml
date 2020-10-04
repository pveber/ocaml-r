open OCamlR

let () = ignore (R.Eval.string "require(stats, quietly=TRUE)")

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
  let open R.Eval in
  call Symbol.rnorm [
    arg id n ;
    opt_arg id "mean" mean ;
    opt_arg id "sd" sd
  ]

let fisher'test ?alternative v v' =
  let open R.Eval in
  call Symbol.fisher'test [
    arg id v ;
    arg id v' ;
    opt_arg id "alternative" alternative ;
  ]

let cor x ?y ?use ?cor_method () =
  let open R.Eval in
  call Symbol.cor [
    arg (fun x -> x) x                   ;
    opt_arg (fun x -> x) "y" y               ;
    opt_arg (fun x -> x) "use" use           ;
    opt_arg (fun x -> x) "method" cor_method ]

let lm formula ?data ?subset ?weights ?na_action ?lm_method ?model ?x ?y ?qr ?singular_ok ?contrasts ?offset () =
  let open R.Eval in
  call Symbol.lm [
    arg (fun x -> x)                formula     ;
    opt_arg (fun x -> x) "data"         data        ;
    opt_arg (fun x -> x) "subset"       subset      ;
    opt_arg (fun x -> x) "weights"      weights     ;
    opt_arg (fun x -> x) "na.action"    na_action   ;
    opt_arg (fun x -> x) "method"       lm_method   ;
    opt_arg (fun x -> x) "model"        model       ;
    opt_arg (fun x -> x) "x"            x           ;
    opt_arg (fun x -> x) "y"            y           ;
    opt_arg (fun x -> x) "qr"           qr          ;
    opt_arg (fun x -> x) "singular.ok"  singular_ok ;
    opt_arg (fun x -> x) "contrasts"    contrasts   ;
    opt_arg (fun x -> x) "offset"       offset      ]

(* let fisher'test_2x2 ?alternative ~ff ~ft ~tf ~tt () = *)
(*   let data = List.map float [ ff ; ft ; tf ; tt ] in *)
(*   let open R.Eval in
     call Stub.fisher'test [ *)
(*     arg (fun x -> matrix ~nrow:2 ~ncol:2 x) data ; *)
(*     opt_arg (fun x -> R.string (string_of_test_kind x)) "alternative" alternative ; *)
(*   ] *)
