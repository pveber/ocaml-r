open OCaml_R
open OCaml_R_base

let () = ignore (R.eval_string "require(graphics, quietly=TRUE)")

module Symbol = struct

  let hist = R.symbol "hist"

end

class hist o = object
  method breaks = R.floats_of_t (R_base.subset2_s o "breaks")
  method counts = R.floats_of_t (R_base.subset2_s o "counts")
  method density = R.floats_of_t (R_base.subset2_s o "density")
  method mids = R.floats_of_t (R_base.subset2_s o "mids")
  method xname = R.string_of_t (R_base.subset2_s o "xname")
  method equidist = R.bool_of_t (R_base.subset2_s o "equidist")
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

let (|?) x f = match x with
  | None -> None
  | Some x -> Some (f x)

let float_tup (x, y) = R.floats [ x ; y ]

let int_tup (x, y) = R.ints [ x ; y ]

let plot ?main ?xlab ?ylab ?xlim ?ylim ~x ?y () =
  R_graphics_stubs.plot
    ?main:(main |? R.string)
    ?xlab:(xlab |? R.string)
    ?ylab:(ylab |? R.string)
    ?xlim:(xlim |? float_tup)
    ?ylim:(ylim |? float_tup)
    ?y:(y |? R.floats)
    (R.floats x)
  |> ignore

let par ?mfrow () =
  R_graphics_stubs.par
    ?mfrow:(mfrow |? int_tup)
    ()
  |> ignore
