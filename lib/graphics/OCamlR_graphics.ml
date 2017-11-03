open OCamlR
open OCamlR_base

let () = ignore (R.eval_string "require(graphics, quietly=TRUE)")

module Symbol = struct

  let hist = R.symbol "hist"
  let plot = R.symbol ~generic:true "plot"
  let par = R.symbol "par"

end

class hist o = object
  method breaks = R.floats_of_t (subset2_s o "breaks")
  method counts = R.floats_of_t (subset2_s o "counts")
  method density = R.floats_of_t (subset2_s o "density")
  method mids = R.floats_of_t (subset2_s o "mids")
  method xname = R.string_of_t (subset2_s o "xname")
  method equidist = R.bool_of_t (subset2_s o "equidist")
end

let any x = (x : _ #R.ty R.t :> < > R.t)

let r_breaks = function
  | `n n -> any (R.int n)
  | `l v -> any (R.floats v)
  | `m `Sturges -> any (R.string "Sturges")
  | `m `Scott -> any (R.string "Scott")
  | `m `FD -> any (R.string "FD")

let hist ?breaks ?freq ?include_lowest ?right ?main ?xlab ?ylab ?xlim ?ylim ?plot x =
  R.eval Symbol.hist [
    R.arg R.floats                  x ;
    R.opt r_breaks     "breaks"         breaks ;
    R.opt R.bool       "freq"           freq ;
    R.opt R.bool       "include_lowest" include_lowest ;
    R.opt R.bool       "right"          right ;
    R.opt R.string     "main"           main;
    R.opt R.string     "xlab"           xlab ;
    R.opt R.string     "ylab"           ylab ;
    R.opt R.float      "xlim"           xlim ;
    R.opt R.float      "ylim"           ylim ;
    R.opt R.bool       "plot"           plot ;
  ]
  |> new hist

let float_tup (x, y) = R.floats [| x ; y |]

let int_tup (x, y) = R.ints [| x ; y |]

type plot_type = [
  | `Points
  | `Lines
  | `Both
  | `Overplotted
  | `Histogram
  | `Stair_steps
  | `Other_steps
  | `Nothing
]

type log_scale = [ `X | `Y | `XY ]

let r_plot_type t =
  R.string (
    match t with
    | `Points -> "p"
    | `Lines -> "l"
    | `Both -> "b"
    | `Overplotted -> "o"
    | `Histogram -> "h"
    | `Stair_steps -> "s"
    | `Other_steps -> "S"
    | `Nothing -> "n"
  )

let r_log_scale t =
  R.string (
    match t with
    | `X -> "x"
    | `Y -> "y"
    | `XY -> "xy"
  )

let plot ?main ?xlab ?ylab ?xlim ?ylim ?plot_type ?lwd ?col ?log ~x ?y () =
  R.eval Symbol.plot [
    R.arg R.floats x ;
    R.opt R.floats "y" y ;
    R.opt R.string "main" main ;
    R.opt R.string "xlab" xlab ;
    R.opt R.string "ylab" ylab ;
    R.opt float_tup "xlim" xlim ;
    R.opt float_tup "ylim" ylim ;
    R.opt r_plot_type "type" plot_type ;
    R.opt R.int "lwd" lwd ;
    R.opt R.string "col" col ;
    R.opt r_log_scale "log" log ;
  ]
  |> ignore

let par ?mfrow () =
  R.eval Symbol.par [
    R.opt int_tup "mfrow" mfrow ;
  ]
  |> ignore
