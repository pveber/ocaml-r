open OCamlR
open OCamlR_base
open OCamlR_stats

let () = ignore (eval_string "require(graphics, quietly=TRUE)")

module Symbol = struct

  let hist = symbol "hist"
  let plot = symbol ~generic:true "plot"
  let boxplot = symbol "boxplot"
  let par = symbol "par"

end

class hist o = object
  method breaks = List_.subset2_exn o "breaks" Dec.floats
  method counts = List_.subset2_exn o "counts" Dec.floats
  method density = List_.subset2_exn o "density" Dec.floats
  method mids = List_.subset2_exn o "mids" Dec.floats
  method xname = List_.subset2_exn o "xname" Dec.string
  method equidist = List_.subset2_exn o "equidist" Dec.bool
end

let r_breaks =
  let open Enc in
  function
  | `n n -> int n
  | `l v -> floats v
  | `m `Sturges -> string "Sturges"
  | `m `Scott -> string "Scott"
  | `m `FD -> string "FD"

let hist ?breaks ?freq ?include_lowest ?right ?(main = "") ?(xlab = "") ?ylab ?xlim ?ylim ?plot x =
  call Symbol.hist Enc.[
      arg floats x ;
      opt_arg r_breaks "breaks" breaks ;
      opt_arg bool "freq" freq ;
      opt_arg bool "include_lowest" include_lowest ;
      opt_arg bool "right"right ;
      arg string ~name:"main" main;
      arg string ~name:"xlab" xlab ;
      opt_arg string "ylab" ylab ;
      opt_arg float  "xlim" xlim ;
      opt_arg float "ylim" ylim ;
      opt_arg bool "plot" plot ;
    ]
  |> List_.unsafe_of_sexp
  |> new hist

let float_tup (x, y) = Enc.floats [| x ; y |]

let int_tup (x, y) = Enc.ints [| x ; y |]

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
  Enc.string (
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
  Enc.string (
    match t with
    | `X -> "x"
    | `Y -> "y"
    | `XY -> "xy"
  )

let plot ?main ?(xlab = "") ?(ylab = "") ?xlim ?ylim ?plot_type ?lwd ?col ?log ~x ?y () =
  call Symbol.plot Enc.[
      arg floats x ;
      opt_arg floats "y" y ;
      opt_arg string "main" main ;
      arg string ~name:"xlab" xlab ;
      arg string ~name:"ylab" ylab ;
      opt_arg float_tup "xlim" xlim ;
      opt_arg float_tup "ylim" ylim ;
      opt_arg r_plot_type "type" plot_type ;
      opt_arg int "lwd" lwd ;
      opt_arg string "col" col ;
      opt_arg r_log_scale "log" log ;
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
  call OCamlR_graphics_stubs2.lines_symbol Enc.[
      arg floats x ;
      opt_arg floats "y" y ;
      opt_arg (fun x -> int (int_of_line_type x)) "lty" lty ;
      opt_arg int "lwd" lwd ;
      opt_arg string "col" col ;
    ]
  |> ignore

let points ?pch ?col ~x ?y () =
  call OCamlR_graphics_stubs2.points_symbol Enc.[
      arg floats x ;
      opt_arg floats "y" y ;
      opt_arg int "pch" pch ;
      opt_arg string "col" col ;
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
  call OCamlR_graphics_stubs2.legend_symbol Enc.[
      arg (fun x -> string (string_of_position x)) x ;
      arg strings legend ;
      opt_arg strings "col" col ;
      opt_arg (fun x -> ints (Array.map int_of_line_type x)) "lty" lty ;
      opt_arg floats "lwd" lwd ;
      opt_arg ints "pch" pch ;
    ]
  |> ignore

let par ?mfrow () =
  call Symbol.par [
      opt_arg int_tup "mfrow" mfrow ;
    ]
  |> ignore

let dataframe_boxplot ?main ?xlab ?ylab formula data =
  call Symbol.boxplot Enc.[
      arg Formula.to_sexp formula ;
      arg Dataframe.to_sexp ~name:"data" data ;
      opt_arg string "main" main ;
      opt_arg string "xlab" xlab ;
      opt_arg string "ylab" ylab ;
    ]
  |> ignore

let list_boxplot ?main ?xlab ?ylab data =
  call Symbol.boxplot Enc.[
      arg List_.to_sexp ~name:"x" data ;
      opt_arg string "main" main ;
      opt_arg string "xlab" xlab ;
      opt_arg string "ylab" ylab ;
    ]
  |> ignore

let abline ?a ?b ?h ?v ?lty ?lwd ?col () =
  call OCamlR_graphics_stubs2.abline_symbol Enc.[
      opt_arg float "a" a ;
      opt_arg float "b" b ;
      opt_arg float "h" h ;
      opt_arg float "v" v ;
      opt_arg (fun x -> int (int_of_line_type x)) "lty" lty ;
      opt_arg int "lwd" lwd ;
      opt_arg string "col" col ;
    ]
  |> ignore
