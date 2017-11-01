open OCaml_R
open OCaml_R_base.R_base

let id x = x
let ( |? ) o f = match o with
  | Some x -> Some (f x)
  | None -> None

module Symbol = struct
  let ks'test = R.symbol "ks.test"
  let p'adjust = R.symbol "p.adjust"
end


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

class fisher'test o = object
  method p'value = R.float_of_t (subset2_s o "p.value")
  method conf'int =
    R.notnil (subset2_s o "conf.int")
    |? (fun x ->
        match R.floats_of_t x with
        | [| x ; y |] -> (x, y)
        | _ -> assert false
      )
  method estimate = R.float_of_t (subset2_s o "estimate")
  method null'value = R.float_of_t (subset2_s o "null.value")
  method alternative = R.string_of_t (subset2_s o "alternative")
  method method_ = R.string_of_t (subset2_s o "method")
  method data'name = R.string_of_t (subset2_s o "data.name")
end

let fisher'test ?alternative v v' =
  R_stats_stubs.fisher'test
    ?alternative:(alternative |? string_of_test_kind |? R.string)
    (R.floats v)
    (R.floats v')
  |> new fisher'test


class ks'test o = object
  method p'value = R.float_of_t (subset2_s o "p.value")
  method statistic = R.float_of_t (subset2_s o "statistic")
  method alternative = R.string_of_t (subset2_s o "alternative")
  method method_ = R.string_of_t (subset2_s o "method")
  method data'name = R.string_of_t (subset2_s o "data.name")
end

let ks'test ?alternative v v' =
  R.eval Symbol.ks'test [
    R.arg R.floats v ;
    R.arg R.floats v' ;
    R.opt (fun x -> R.string (string_of_test_kind x)) "alternative" alternative ;
  ]
  |> new ks'test

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
    R.eval Symbol.p'adjust [
      R.arg R.floats data ;
      R.opt (fun x -> R.string (string_of_p'adjust_method x)) "method" method_
    ]
  )
