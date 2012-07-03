let () = ignore (R.eval_string "require(grDevices, quietly=TRUE)")

module Stub = struct

  let png = R.symbol "png"

  let pdf = R.symbol "pdf"

  let dev_off = R.symbol "dev.off"

  (* TODO: This segfaults: let dev = R.symbol "dev" *)

end

type length_unit = [`pixel | `inch | `cm | `mm]

let string_of_length_unit = function
| `pixel -> "px"
| `inch -> "in"
| `cm -> "cm"
| `mm -> "mm"

let r_length_unit x = R.string (string_of_length_unit x)

let png ?width ?height ?unit ?pointsize path = 
  ignore (
    R.eval Stub.png [
      R.arg R.string                  path ;
      R.opt R.float       "width"     width ;
      R.opt R.float       "height"    height ;
      R.opt r_length_unit "unit"      unit ;
      R.opt R.int         "pointsize" pointsize
    ])

let pdf ?width ?height ?pointsize path = 
  ignore (
    R.eval Stub.pdf [
      R.arg R.string                  path ;
      R.opt R.float       "width"     width ;
      R.opt R.float       "height"    height ;
      R.opt R.int         "pointsize" pointsize
    ])

let dev_off () = 
  ignore (
    R.eval Stub.dev_off []
  )
