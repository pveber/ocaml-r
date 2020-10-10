open OCamlR

let () = ignore (eval_string "require(stats, quietly=TRUE)")

let id x = x

module Symbol = struct
  let rnorm = symbol "rnorm"
  let dnorm = symbol "dnorm"
  let pnorm = symbol "pnorm"
  let qnorm = symbol "qnorm"

  (* The log normal distribution. *)
  let dlnorm = symbol "dlnorm"
  let plnorm = symbol "plnorm"
  let qlnorm = symbol "qlnorm"
  let rlnorm = symbol "rlnorm"

  let cor = symbol "cor"
  let lm = symbol "lm"
  let stl = symbol "stl"

  let fisher'test = symbol "fisher.test"
  let poisson_test = symbol "poisson.test"
  let shapiro_test = symbol "shapiro.test"
  let fitted = symbol "fitted"
  let sSgompertz = symbol "SSgompertz"
end

let rnorm ?mean ?sd n =
  call Symbol.rnorm [
    arg id n ;
    opt_arg id "mean" mean ;
    opt_arg id "sd" sd
  ]

let fisher'test ?alternative v v' =
  call Symbol.fisher'test [
    arg id v ;
    arg id v' ;
    opt_arg id "alternative" alternative ;
  ]

let cor x ?y ?use ?cor_method () =
  call Symbol.cor [
    arg (fun x -> x) x                   ;
    opt_arg (fun x -> x) "y" y               ;
    opt_arg (fun x -> x) "use" use           ;
    opt_arg (fun x -> x) "method" cor_method ]

let lm formula ?data ?subset ?weights ?na_action ?lm_method ?model ?x ?y ?qr ?singular_ok ?contrasts ?offset () =
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
(*   let open Eval in
     call Stub.fisher'test [ *)
(*     arg (fun x -> matrix ~nrow:2 ~ncol:2 x) data ; *)
(*     opt_arg (fun x -> string (string_of_test_kind x)) "alternative" alternative ; *)
(*   ] *)
