open OCamlR
open OCamlR_wraputils
open OCamlR_base

module Stubs = OCamlR_utils_stubs

let data ?envir name =
  R.eval (R.symbol "data") [
    R.arg R.string name ;
    R.opt Environment.r "envir" envir ;
  ]
  |> ignore

let write'table ?file ?sep ?col'names ?row'names ?quote x =
  Stubs.write'table
    ?file:(file |?> R.string)
    ?sep:(sep |?> R.string)
    ?col'names:(col'names |?> R.bool)
    ?row'names:(row'names |?> R.bool)
    ?quote:(quote |?> R.bool)
    ~x:(Dataframe.r x) ()
  |> ignore
