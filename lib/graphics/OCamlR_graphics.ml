open OCamlR
open OCamlR_base
open OCamlR_stats

let () = ignore (R.eval_string "require(graphics, quietly=TRUE)")

module Symbol = struct

  let hist = R.symbol "hist"
  let plot = R.symbol ~generic:true "plot"
  let boxplot = R.symbol "boxplot"
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

let hist ?breaks ?freq ?include_lowest ?right ?(main = "") ?(xlab = "") ?ylab ?xlim ?ylim ?plot x =
  R.eval Symbol.hist [
    R.arg R.floats x ;
    R.opt r_breaks "breaks" breaks ;
    R.opt R.bool "freq" freq ;
    R.opt R.bool "include_lowest" include_lowest ;
    R.opt R.bool "right"right ;
    R.arg R.string ~name:"main" main;
    R.arg R.string ~name:"xlab" xlab ;
    R.opt R.string "ylab" ylab ;
    R.opt R.float  "xlim" xlim ;
    R.opt R.float "ylim" ylim ;
    R.opt R.bool "plot" plot ;
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

let plot ?main ?(xlab = "") ?(ylab = "") ?xlim ?ylim ?plot_type ?lwd ?col ?log ~x ?y () =
  R.eval Symbol.plot [
    R.arg R.floats x ;
    R.opt R.floats "y" y ;
    R.opt R.string "main" main ;
    R.arg R.string ~name:"xlab" xlab ;
    R.arg R.string ~name:"ylab" ylab ;
    R.opt float_tup "xlim" xlim ;
    R.opt float_tup "ylim" ylim ;
    R.opt r_plot_type "type" plot_type ;
    R.opt R.int "lwd" lwd ;
    R.opt R.string "col" col ;
    R.opt r_log_scale "log" log ;
  ]
  |> ignore

type line_type = [
  | `blank
  | `solid
  | `dashed
  | `dotted
  | `dotdash
  | `longdash
  | `twodash
]

let int_of_line_type = function
  | `blank -> 0
  | `solid -> 1
  | `dashed -> 2
  | `dotted -> 3
  | `dotdash -> 4
  | `longdash -> 5
  | `twodash -> 6

let lines ?lty ?lwd ?col ~x ?y () =
  R.eval OCamlR_graphics_stubs2.lines_symbol [
    R.arg R.floats x ;
    R.opt R.floats "y" y ;
    R.opt (fun x -> R.int (int_of_line_type x)) "lty" lty ;
    R.opt R.int "lwd" lwd ;
    R.opt R.string "col" col ;
  ]
  |> ignore

let string_of_position = function
  | `bottomright -> "bottomright"
  | `bottom -> "bottom"
  | `bottomleft -> "bottomleft"
  | `left -> "left"
  | `topleft -> "topleft"
  | `top -> "top"
  | `topright -> "topright"
  | `right -> "right"
  | `center -> "center"

let legend ?col ?lty ?lwd ?pch x legend =
  R.eval OCamlR_graphics_stubs2.legend_symbol [
    R.arg (fun x -> R.string (string_of_position x)) x ;
    R.arg R.strings legend ;
    R.opt R.strings "col" col ;
    R.opt (fun x -> R.ints (Array.map int_of_line_type x)) "lty" lty ;
    R.opt R.floats "lwd" lwd ;
    R.opt R.ints "pch" pch ;
  ]
  |> ignore

let par ?mfrow () =
  R.eval Symbol.par [
    R.opt int_tup "mfrow" mfrow ;
  ]
  |> ignore

let dataframe_boxplot ?main ?xlab ?ylab formula data =
  R.eval Symbol.boxplot [
    R.arg Formula.r formula ;
    R.arg Dataframe.r ~name:"data" data ;
    R.opt R.string "main" main ;
    R.opt R.string "xlab" xlab ;
    R.opt R.string "ylab" ylab ;
  ]
  |> ignore

let abline ?a ?b ?h ?v ?lty ?lwd ?col () =
  R.eval OCamlR_graphics_stubs2.abline_symbol [
    R.opt R.float "a" a ;
    R.opt R.float "b" b ;
    R.opt R.float "h" h ;
    R.opt R.float "v" v ;
    R.opt (fun x -> R.int (int_of_line_type x)) "lty" lty ;
    R.opt R.int "lwd" lwd ;
    R.opt R.string "col" col ;
  ]
  |> ignore
