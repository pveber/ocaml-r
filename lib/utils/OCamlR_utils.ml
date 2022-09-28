open OCamlR
open OCamlR_base

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

let read_table_symbol = symbol "read.table"

let read'table
    ?header ?sep ?quote ?dec
    ?numerals ?row'names ?col'names
    ?na'strings ?check'names ?strip'white
    ?comment'char ?stringsAsFactors file =
  call read_table_symbol Enc.[
      arg ~name:"file" string file ;
      opt_arg bool "header" header ;
      opt_arg string "sep" sep ;
      opt_arg string "quote" quote ;
      opt_arg string "dec" dec ;
      opt_arg r_numerals "numerals" numerals ;
      opt_arg bool "col.names" col'names ;
      opt_arg bool "row.names" row'names ;
      opt_arg string "na.strings" na'strings ;
      opt_arg bool "check.names" check'names ;
      opt_arg bool "strip.white" strip'white ;
      opt_arg string "comment.char" comment'char ;
      opt_arg bool "stringsAsFactors" stringsAsFactors ;
    ]
  |> Dataframe.unsafe_of_sexp

let write_table_symbol = symbol "write.table"

let write'table ?file ?sep ?col'names ?row'names ?quote x =
  call write_table_symbol Enc.[
      arg Dataframe.to_sexp x ;
      opt_arg string "file" file ;
      opt_arg string "sep" sep ;
      opt_arg bool "col.names" col'names ;
      opt_arg bool "row.names" row'names ;
      opt_arg bool "quote" quote ;
    ]
  |> ignore
