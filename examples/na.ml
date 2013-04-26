#require "R.base";;

let x = R.optfloats [ None ; None ; Some 3.14159 ; None ]

let print = 
  let symbol = R.symbol "print" in
  fun x -> R.eval symbol [ R.arg (fun x -> x) x ]

let _ = print x




















