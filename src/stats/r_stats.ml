let () = ignore (R.eval_string "require(stats, quietly=TRUE)")

open R_base

let id x = x
let ( |? ) o f = match o with
  | Some x -> Some (f x)
  | None -> None


let rnorm ?mean ?sd n =
  R_stats_stubs.rnorm
    ?mean:(mean |? R.float)
    ?sd:(sd |? R.float)
    (n |> R.int)
  |> R.floats_of_t


let string_of_test_kind = function
  | `two_sided -> "two.sided"
  | `greater -> "greater"
  | `less -> "less"

(* class fisher'test o = object *)
(*   inherit list_ o *)
(*   method p'value = R.floats_of_t (o#get_s "p.value") *)
(*   method conf'int = ( *)
(*     match R.notnil (o # get_s "conf.int") with *)
(*         | None -> None *)
(*         | Some x -> ( *)
(*             match (reals x)#to_floats with *)
(*             | [ x ; y ] -> Some (x, y) *)
(*             | _ -> assert false *)
(*           ) *)
(*   ) *)
(*   method estimate = R.float_of_t (o#gest_s "estimate") *)
(*     (\* method null'value = R.float_of_t (o ## null'value) *\) *)
(*     (\* method alternative = R.string_of_t (o ## alternative) *\) *)
(*     (\* method method_ = R.string_of_t (R_base.subset2_s o "method") *\) *)
(*     (\* method data'name = R.string_of_t (o ## data'name) *\) *)
(* end *)

(* let fisher'test_sym = R.symbol "fisher.test" *)

(* let fisher'test ?alternative v v' = *)
(*   R.eval fisher'test_sym [ *)
(*       R.arg R.floats v ; *)
(*       R.arg R.floats v' ; *)
(*       R.opt (fun x -> R.string (string_of_test_kind x)) "alternative" alternative ; *)
(*     ] *)



(* module Stub = struct *)

(*   (\* Normal distribution. *\) *)
(*   let rnorm = *)
(*     let sym = R.symbol "rnorm" in *)
(*     fun ?mean ?sd n -> R.eval sym [ *)
(*         R.arg id        n ; *)
(*         R.opt id "mean" mean ; *)
(*         R.opt id "sd"   sd ; *)
(*       ] *)

(*   let dnorm = R.symbol "dnorm" *)
(*   let pnorm = R.symbol "pnorm" *)
(*   let qnorm = R.symbol "qnorm" *)




(*   let cor = R.symbol "cor" *)

(*   let cor x ?y ?use ?cor_method () = *)
(*     R.eval cor [ *)
(*       R.arg (fun x -> x) x                   ; *)
(*       R.opt (fun x -> x) "y" y               ; *)
(*       R.opt (fun x -> x) "use" use           ; *)
(*       R.opt (fun x -> x) "method" cor_method ] *)

(*   let lm = R.symbol "lm" *)
(*   let lm formula ?data ?subset ?weights ?na_action ?lm_method ?model ?x ?y ?qr ?singular_ok ?contrasts ?offset () = *)
(*     R.eval lm [ *)
(*     R.arg (fun x -> x)                formula     ; *)
(*     R.opt (fun x -> x) "data"         data        ; *)
(*     R.opt (fun x -> x) "subset"       subset      ; *)
(*     R.opt (fun x -> x) "weights"      weights     ; *)
(*     R.opt (fun x -> x) "na.action"    na_action   ; *)
(*     R.opt (fun x -> x) "method"       lm_method   ; *)
(*     R.opt (fun x -> x) "model"        model       ; *)
(*     R.opt (fun x -> x) "x"            x           ; *)
(*     R.opt (fun x -> x) "y"            y           ; *)
(*     R.opt (fun x -> x) "qr"           qr          ; *)
(*     R.opt (fun x -> x) "singular.ok"  singular_ok ; *)
(*     R.opt (fun x -> x) "contrasts"    contrasts   ; *)
(*     R.opt (fun x -> x) "offset"       offset      ] *)

(*   let stl = R.symbol "stl" *)

(*   let ks'test = R.symbol "ks.test" *)


(*   let poisson_test = R.symbol "poisson.test" *)

(*   let shapiro_test = R.symbol "shapiro.test" *)

(*   let fitted = R.symbol "fitted" *)

(*   let p'adjust = R.symbol "p.adjust" *)

(*   let sSgompertz = R.symbol "SSgompertz" *)


(*   (\* The log normal distribution. *\) *)
(*   let dlnorm = R.symbol "dlnorm" *)
(*   let plnorm = R.symbol "plnorm" *)
(*   let qlnorm = R.symbol "qlnorm" *)
(*   let rlnorm = R.symbol "rlnorm" *)

(* end *)

(* let rnorm ?mean ?sd n = *)
(*   Stub.rnorm ?mean:(mean |? R.float) ?sd:(sd |? R.float) (R.int n) *)
(*   |> R.floats_of_t *)



(* (\* let fisher'test_2x2 ?alternative ~ff ~ft ~tf ~tt () = *\) *)
(* (\*   let data = List.map float [ ff ; ft ; tf ; tt ] in *\) *)
(* (\*   R.eval Stub.fisher'test [ *\) *)
(* (\*     R.arg (fun x -> matrix ~nrow:2 ~ncol:2 x) data ; *\) *)
(* (\*     R.opt (fun x -> R.string (string_of_test_kind x)) "alternative" alternative ; *\) *)
(* (\*   ] *\) *)

(* let ks'test ?alternative v v' = *)
(*   R.eval Stub.ks'test [ *)
(*     R.arg R.floats v ; *)
(*     R.arg R.floats v' ; *)
(*     R.opt (fun x -> R.string (string_of_test_kind x)) "alternative" alternative ; *)
(*   ] *)

(* let string_of_p'adjust_method = function *)
(* | `fdr -> "fdr" *)
(* | `holm -> "holm" *)
(* | `hochberg -> "hochberg" *)
(* | `hommel -> "hommel" *)
(* | `bonferroni -> "bonferroni" *)
(* | `BH -> "BH" *)
(* | `BY -> "BY" *)

(* let p'adjust ?method_ data = *)
(*   R.floats_of_t ( *)
(*     R.eval Stub.p'adjust [ *)
(*       R.arg R.floats data ; *)
(*       R.opt (fun x -> R.string (string_of_p'adjust_method x)) "method" method_ *)
(*     ] *)
(*   ) *)


















