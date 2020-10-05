open OCamlR
open OCamlR_base
open OCamlR_wraputils

module S = OCamlR_stats_stubs2

let o x f = match x with
  | None -> None
  | Some x -> Some (f x)

let float_tup (x, y) = Enc.floats [| x ; y |]

module Symbol = struct
  let ks'test = symbol "ks.test"
  let p'adjust = symbol "p.adjust"
end

module Formula = struct
  include Langsxp
  let of_string x =
    S.formula ~x:(Enc.string x) ()
    |> unsafe_of_sexp
end


let rnorm ?mean ?sd n =
  S.rnorm
    ?mean:(mean |?> Enc.float)
    ?sd:(sd |?> Enc.float)
    ~n:(n |> Enc.int)
    ()
  |> Dec.floats


let string_of_test_kind = function
  | `two_sided -> "two.sided"
  | `greater -> "greater"
  | `less -> "less"

class type test = object
  method p'value : float
  method method_ : string
  method data'name : string
  method alternative : string
end

class test_impl o = object
  method p'value = List_.subset2_exn o "p.value" Dec.float
  method method_ = List_.subset2_exn o "method" Dec.string
  method data'name = List_.subset2_exn o "data.name" Dec.string
  method alternative = List_.subset2_exn o "alternative" Dec.string
end

class fisher'test o = object
  inherit test_impl o
  method conf'int =
    List_.subset2 o "conf.int" Dec.floats
    |> Option.map (function
        | [| x ; y |] -> (x, y)
        | _ -> assert false
      )
  method estimate = List_.subset2_exn o "estimate" Dec.float
  method null'value = List_.subset2_exn o "null.value" Dec.float
end

let fisher'test ?alternative v v' =
  S.fisher'test
    ?alternative:(alternative |?> string_of_test_kind |?> Enc.string)
    ~x:(Logical.to_sexp v)
    ~y:(Logical.to_sexp v')
    ()
  |> List_.unsafe_of_sexp
  |> new fisher'test

class test_impl_with_statistic o = object
  inherit test_impl o
  method statistic = List_.subset2_exn o "statistic" Dec.float
end

let chisq'test_contingency_table ?correct ?simulate'p'value ?b mat =
  S.chisq'test
    ~x:(Matrix.to_sexp mat)
    ?correct:(o correct Enc.bool)
    ?simulate'p'value:(o simulate'p'value Enc.bool)
    ?_B:(o b Enc.int)
    ()
  |> List_.unsafe_of_sexp
  |> new test_impl_with_statistic

class ks'test o = object
  inherit test_impl_with_statistic o
end

let ks'test ?alternative v v' =
  let open Eval in
  call Symbol.ks'test Enc.[
    arg floats v ;
    arg floats v' ;
    opt_arg (fun x -> string (string_of_test_kind x)) "alternative" alternative ;
  ]
  |> List_.unsafe_of_sexp
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
  let open Eval in
  call Symbol.p'adjust Enc.[
      arg floats data ;
      opt_arg (fun x -> string (string_of_p'adjust_method x)) "method" method_
    ]
  |> Dec.floats

module Ecdf = struct
  type t = List_.t
  let make x =
    OCamlR_stats_stubs2.ecdf ~x:(Numeric.to_sexp x) ()
    |> List_.unsafe_of_sexp

  let plot ?(main = "") ?xlab ?ylab ?xlim ?ylim o =
    let open Eval in
    call OCamlR_stats_stubs2.plot'ecdf_symbol Enc.[
      arg ~name:"x" List_.to_sexp o ;
      opt_arg string "xlab" xlab ;
      opt_arg string "ylab" ylab ;
      arg string ~name:"main" main ;
      opt_arg float_tup "xlim" xlim ;
      opt_arg float_tup "ylim" ylim ;
    ]
    |> ignore
end
