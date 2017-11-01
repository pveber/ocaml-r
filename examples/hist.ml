#require "OCaml_R.grDevices"
#require "OCaml_R.graphics"
#require "OCaml_R.stats"

open OCaml_R_grDevices
open OCaml_R_stats
open OCaml_R_graphics

let () = R_grDevices.png ~width:500. ~height:500. "delme.png"
let x = R_stats.rnorm 100
let h = R_graphics.hist ~main:"" ~xlab:"x" x
let () = R_grDevices.dev_off ()
