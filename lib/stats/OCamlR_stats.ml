open OCamlR
open OCamlR_base

let float_tup (x, y) = Enc.floats [| x ; y |]

module Symbol = struct
  let ks'test = symbol "ks.test"
  let p'adjust = symbol "p.adjust"
end

let formula_stub = symbol "formula"

let formula x =
  call formula_stub [ arg Enc.string x ]
  |> Formula.unsafe_of_sexp

let rnorm_symbol = symbol "rnorm"

let rnorm ?mean ?sd n =
  call rnorm_symbol Enc.[
      arg ~name:"n" int n ;
      opt_arg float "mean" mean ;
      opt_arg float "sd" sd ;
    ]
  |> Numeric.unsafe_of_sexp


let string_of_test_kind = function
  | `two_sided -> "two.sided"
  | `greater -> "greater"
  | `less -> "less"

let enc_test_kind x = Enc.string (string_of_test_kind x)

module type Test = sig
  include module type of List_
  val p'value : t -> float
  val _method_ : t -> string
  val data'name : t -> string
  val alternative : t -> string
end

(**
   Note that not all access operations really are available,
   the selection is performed via signature shadowing.
*)
module Test_impl = struct
  include List_

  let p'value o = List_.subset2_exn o "p.value" Dec.float
  let _method_ o = List_.subset2_exn o "method" Dec.string
  let data'name o = List_.subset2_exn o "data.name" Dec.string
  let alternative o = List_.subset2_exn o "alternative" Dec.string
  let conf'int o =
    List_.subset2 o "conf.int" Dec.floats
    |> Option.map (function
        | [| x ; y |] -> (x, y)
        | _ -> assert false
      )
  let estimate o = List_.subset2_exn o "estimate" Dec.float
  let null'value o = List_.subset2_exn o "null.value" Dec.float
  let statistic o = List_.subset2_exn o "statistic" Dec.float
end

module Fisher'test = struct
  include Test_impl

  let fisher_test_symbol = symbol "fisher.test"

  let logicals ?alternative v v' =
    call fisher_test_symbol [
      arg ~name:"x" Logical.to_sexp v ;
      arg ~name:"y" Logical.to_sexp v' ;
      opt_arg enc_test_kind "alternative" alternative ;
    ]
    |> List_.unsafe_of_sexp
end

module T'test = struct
  include Test_impl

  let t_test_symbol = symbol "t.test"

  let one_sample ?alternative x =
    call t_test_symbol [
      opt_arg enc_test_kind "alternative" alternative ;
      arg ~name:"x" Numeric.to_sexp x ;
    ]
    |> List_.unsafe_of_sexp
end

module Chisq'test = struct
  include Test_impl

  let chisq_test_symbol = symbol "chisq.test"

  let contingency_table ?correct ?simulate'p'value ?b mat =
    call chisq_test_symbol Enc.[
      arg ~name:"x" Integer.Matrix.to_sexp mat ;
      opt_arg bool "correct" correct ;
      opt_arg bool "simulate.p.value" simulate'p'value ;
      opt_arg int "B" b ;
    ]
    |> List_.unsafe_of_sexp
end

module Ks'test = struct
  include Test_impl

  let make ?alternative v v' =
    call Symbol.ks'test Enc.[
        arg Numeric.to_sexp v ;
        arg Numeric.to_sexp v' ;
        opt_arg (fun x -> string (string_of_test_kind x)) "alternative" alternative ;
      ]
    |> List_.unsafe_of_sexp
end

let enc_p'adjust_method x =
  Enc.string (
    match x with
    | `fdr -> "fdr"
    | `holm -> "holm"
    | `hochberg -> "hochberg"
    | `hommel -> "hommel"
    | `bonferroni -> "bonferroni"
    | `BH -> "BH"
    | `BY -> "BY"
  )

let p'adjust ?method_ data =
  call Symbol.p'adjust [
      arg Numeric.to_sexp data ;
      opt_arg enc_p'adjust_method "method" method_ ;
    ]
  |> Numeric.unsafe_of_sexp

module Ecdf = struct
  type t = List_.t

  let ecdf_symbol = symbol "ecdf"

  let make x =
    call ecdf_symbol [ arg Numeric.to_sexp x ]
    |> List_.unsafe_of_sexp

  let plot_ecdf_symbol = symbol "plot.ecdf"

  let plot ?(main = "") ?xlab ?ylab ?xlim ?ylim o =
    call plot_ecdf_symbol Enc.[
      arg ~name:"x" List_.to_sexp o ;
      opt_arg string "xlab" xlab ;
      opt_arg string "ylab" ylab ;
      arg string ~name:"main" main ;
      opt_arg float_tup "xlim" xlim ;
      opt_arg float_tup "ylim" ylim ;
    ]
    |> ignore
end

let qqplot_symbol = symbol "qqplot"

let qqplot ?main ?(xlab = "") ?(ylab = "") ?plot_type ?lwd ?col x y =
  call qqplot_symbol Enc.[
      arg floats x ;
      arg floats y ;
      opt_arg string "main" main ;
      arg string ~name:"xlab" xlab ;
      arg string ~name:"ylab" ylab ;
      opt_arg OCamlR_graphics.Enc.plot_type "type" plot_type ;
      opt_arg int "lwd" lwd ;
      opt_arg string "col" col ;
    ]
  |> ignore
