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

let r_numerals o =
  R.string (
    match o with
    | `allow'loss -> "allow.loss"
    | `warn'loss -> "warn.loss"
    | `no'loss -> "no.loss"
  )

let read'table
    ?header ?sep ?quote ?dec
    ?numerals ?row'names ?col'names
    ?na'strings ?check'names ?strip'white
    ?comment'char ?stringsAsFactors file =
  Stubs.read'table
    ~file:(file |> R.string)
    ?header:(header |?> R.bool)
    ?sep:(sep |?> R.string)
    ?quote:(quote |?> R.string)
    ?dec:(dec |?> R.string)
    ?numerals:(numerals |?> r_numerals)
    ?col'names:(col'names |?> R.bool)
    ?row'names:(row'names |?> R.bool)
    ?na'strings:(na'strings |?> R.string)
    ?check'names:(check'names |?> R.bool)
    ?strip'white:(strip'white |?> R.bool)
    ?comment'char:(comment'char |?> R.string)
    ?stringsAsFactors:(stringsAsFactors |?> R.bool)
    ()
  |> Dataframe.Unsafe.of_r

let write'table ?file ?sep ?col'names ?row'names ?quote x =
  Stubs.write'table
    ?file:(file |?> R.string)
    ?sep:(sep |?> R.string)
    ?col'names:(col'names |?> R.bool)
    ?row'names:(row'names |?> R.bool)
    ?quote:(quote |?> R.bool)
    ~x:(Dataframe.r x) ()
  |> ignore
