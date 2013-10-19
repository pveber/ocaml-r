#require "R.grDevices"
#require "R.graphics"
#require "R.stats"

open RgrDevices

let () = png ~width:500. ~height:500. "delme.png"
let x = Rstats.rnorm 100
let h = Rgraphics.hist x
let () = dev_off ()
