open OCamlR

let () = ignore (R.Eval.string "require(grDevices, quietly=TRUE)")

module Stub = struct

  let png = R.symbol "png"

  let pdf = R.symbol "pdf"

  let postscript = R.symbol "postscript"

  let svg = R.symbol "svg"

  let dev_off = R.symbol "dev.off"

  (* TODO: This segfaults: let dev = R.symbol "dev" *)

end

type length_unit = [`pixel | `inch | `cm | `mm]

let string_of_length_unit = function
  | `pixel -> "px"
  | `inch -> "in"
  | `cm -> "cm"
  | `mm -> "mm"

let r_length_unit x = R.Enc.string (string_of_length_unit x)

let png ?width ?height ?unit ?pointsize path =
  ignore (
    let open R.Eval in
    call Stub.png [
      arg R.Enc.string                  path ;
      opt_arg R.Enc.float       "width"     width ;
      opt_arg R.Enc.float       "height"    height ;
      opt_arg r_length_unit "unit"      unit ;
      opt_arg R.Enc.int         "pointsize" pointsize
    ])

let pdf ?width ?height ?pointsize path =
  ignore (
    let open R.Eval in
    call Stub.pdf [
      arg R.Enc.string                  path ;
      opt_arg R.Enc.float       "width"     width ;
      opt_arg R.Enc.float       "height"    height ;
      opt_arg R.Enc.int         "pointsize" pointsize
    ])

let postscript ?width ?height ?pointsize path =
  ignore (
    let open R.Eval in
    call Stub.postscript [
      arg R.Enc.string                  path ;
      opt_arg R.Enc.float       "width"     width ;
      opt_arg R.Enc.float       "height"    height ;
      opt_arg R.Enc.int         "pointsize" pointsize
    ])

let svg ?width ?height ?pointsize path =
  ignore (
    let open R.Eval in
    call Stub.svg [
      arg R.Enc.string                  path ;
      opt_arg R.Enc.float       "width"     width ;
      opt_arg R.Enc.float       "height"    height ;
      opt_arg R.Enc.int         "pointsize" pointsize
    ])

let dev_off () =
  ignore (
    let open R.Eval in
    call Stub.dev_off []
  )
