open OCamlR

let () = ignore (Eval.string "require(grDevices, quietly=TRUE)")

module Stub = struct

  let png = symbol "png"

  let pdf = symbol "pdf"

  let postscript = symbol "postscript"

  let svg = symbol "svg"

  let dev_off = symbol "dev.off"

  (* TODO: This segfaults: let dev = symbol "dev" *)

end

type length_unit = [`pixel | `inch | `cm | `mm]

let string_of_length_unit = function
  | `pixel -> "px"
  | `inch -> "in"
  | `cm -> "cm"
  | `mm -> "mm"

let r_length_unit x = Enc.string (string_of_length_unit x)

let png ?width ?height ?unit ?pointsize path =
  ignore (
    let open Eval in
    call Stub.png [
      arg Enc.string                  path ;
      opt_arg Enc.float       "width"     width ;
      opt_arg Enc.float       "height"    height ;
      opt_arg r_length_unit "unit"      unit ;
      opt_arg Enc.int         "pointsize" pointsize
    ])

let pdf ?width ?height ?pointsize path =
  ignore (
    let open Eval in
    call Stub.pdf [
      arg Enc.string                  path ;
      opt_arg Enc.float       "width"     width ;
      opt_arg Enc.float       "height"    height ;
      opt_arg Enc.int         "pointsize" pointsize
    ])

let postscript ?width ?height ?pointsize path =
  ignore (
    let open Eval in
    call Stub.postscript [
      arg Enc.string                  path ;
      opt_arg Enc.float       "width"     width ;
      opt_arg Enc.float       "height"    height ;
      opt_arg Enc.int         "pointsize" pointsize
    ])

let svg ?width ?height ?pointsize path =
  ignore (
    let open Eval in
    call Stub.svg [
      arg Enc.string                  path ;
      opt_arg Enc.float       "width"     width ;
      opt_arg Enc.float       "height"    height ;
      opt_arg Enc.int         "pointsize" pointsize
    ])

let dev_off () =
  ignore (
    let open Eval in
    call Stub.dev_off []
  )
