open OCamlR
open OCamlR_base
open OCamlR_wraputils

module S = OCamlR_stats_stubs2

let o x f = match x with
  | None -> None
  | Some x -> Some (f x)

module Symbol = struct
  let ks'test = R.symbol "ks.test"
  let p'adjust = R.symbol "p.adjust"
end

module Formula = struct
  include S3
  let of_string x =
    S.formula ~x:(R.string x) ()
    |> unsafe_of_r
end


let rnorm ?mean ?sd n =
  S.rnorm
    ?mean:(mean |?> R.float)
    ?sd:(sd |?> R.float)
    ~n:(n |> R.int)
    ()
  |> R.floats_of_t


let string_of_test_kind = function
  | `two_sided -> "two.sided"
  | `greater -> "greater"
  | `less -> "less"

class test_result o = object
  method p'value = R.float_of_t (subset2_s o "p.value")
  method method_ = R.string_of_t (subset2_s o "method")
  method data'name = R.string_of_t (subset2_s o "data.name")
end

class fisher'test o = object
  inherit test_result o
  method conf'int =
    R.notnil (subset2_s o "conf.int")
    |?> (fun x ->
        match R.floats_of_t x with
        | [| x ; y |] -> (x, y)
        | _ -> assert false
      )
  method estimate = R.float_of_t (subset2_s o "estimate")
  method null'value = R.float_of_t (subset2_s o "null.value")
  method alternative = R.string_of_t (subset2_s o "alternative")
end

let fisher'test ?alternative v v' =
  S.fisher'test
    ?alternative:(alternative |?> string_of_test_kind |?> R.string)
    ~x:(Logical.r v)
    ~y:(Logical.r v')
    ()
  |> new fisher'test

class chipsq'test o = object
  inherit test_result o
  method statistic = R.float_of_t (subset2_s o "statistic")
end

let chisq'test_contingency_table ?correct ?simulate'p'value ?b mat =
  S.chisq'test
    ~x:(Matrix.r mat)
    ?correct:(o correct R.bool)
    ?simulate'p'value:(o simulate'p'value R.bool)
    ?_B:(o b R.int)
    ()
  |> new chipsq'test

class ks'test o = object
  inherit test_result o
  method statistic = R.float_of_t (subset2_s o "statistic")
  method alternative = R.string_of_t (subset2_s o "alternative")
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

module Ecdf = struct
  type t = < > R.t
  let make x = OCamlR_stats_stubs2.ecdf ~x:(Numeric.r x) ()
  let plot ?(main = "") ?ylab o =
    R.eval OCamlR_stats_stubs2.plot'ecdf_symbol [
      R.arg ~name:"x" ident o ;
      R.opt R.string "ylab" ylab ;
      R.arg R.string ~name:"main" main ;
    ]
    |> ignore
end
