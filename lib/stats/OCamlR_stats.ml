open OCamlR
open OCamlR.Eval
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
  |> Numeric.unsafe_of_sexp


let string_of_test_kind = function
  | `two_sided -> "two.sided"
  | `greater -> "greater"
  | `less -> "less"

let enc_test_kind x = Enc.string (string_of_test_kind x)

module type Test = sig
  type t
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
  type t = List_.t

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

  let logicals ?alternative v v' =
    S.fisher'test
      ?alternative:(alternative |?> string_of_test_kind |?> Enc.string)
      ~x:(Logical.to_sexp v)
      ~y:(Logical.to_sexp v')
      ()
    |> List_.unsafe_of_sexp
end

module T'test = struct
  include Test_impl

  let one_sample ?alternative x =
    call S.t'test_symbol [
      opt_arg enc_test_kind "alternative" alternative ;
      arg ~name:"x" Numeric.to_sexp x ;
    ]
    |> List_.unsafe_of_sexp
end

module Chisq'test = struct
  include Test_impl

  let contingency_table ?correct ?simulate'p'value ?b mat =
    S.chisq'test
      ~x:(Matrix.to_sexp mat)
      ?correct:(o correct Enc.bool)
      ?simulate'p'value:(o simulate'p'value Enc.bool)
      ?_B:(o b Enc.int)
      ()
    |> List_.unsafe_of_sexp
end

module Ks'test = struct
  include Test_impl

  let make ?alternative v v' =
    let open Eval in
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
  let open Eval in
  call Symbol.p'adjust [
      arg Numeric.to_sexp data ;
      opt_arg enc_p'adjust_method "method" method_ ;
    ]
  |> Numeric.unsafe_of_sexp

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
