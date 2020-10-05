open OCamlR
open OCamlR_wraputils
open OCamlR_base

module Stubs = OCamlR_utils_stubs

let data ?envir name =
  call (symbol "data") Enc.[
    arg string name ;
    opt_arg Environment.to_sexp "envir" envir ;
  ]
  |> ignore

let r_numerals o =
  Enc.string (
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
  let open Enc in
  Stubs.read'table
    ~file:(file |> string)
    ?header:(header |?> bool)
    ?sep:(sep |?> string)
    ?quote:(quote |?> string)
    ?dec:(dec |?> string)
    ?numerals:(numerals |?> r_numerals)
    ?col'names:(col'names |?> bool)
    ?row'names:(row'names |?> bool)
    ?na'strings:(na'strings |?> string)
    ?check'names:(check'names |?> bool)
    ?strip'white:(strip'white |?> bool)
    ?comment'char:(comment'char |?> string)
    ?stringsAsFactors:(stringsAsFactors |?> bool)
    ()
  |> Dataframe.unsafe_of_sexp

let write'table ?file ?sep ?col'names ?row'names ?quote x =
  let open Enc in
  Stubs.write'table
    ?file:(file |?> string)
    ?sep:(sep |?> string)
    ?col'names:(col'names |?> bool)
    ?row'names:(row'names |?> bool)
    ?quote:(quote |?> bool)
    ~x:(Dataframe.to_sexp x) ()
  |> ignore
