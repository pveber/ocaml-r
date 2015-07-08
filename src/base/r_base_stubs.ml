let id x = x

module Symbol = struct
  let length = R.symbol ~generic:true "length"

  let dim = R.symbol ~generic:true "dim"

  let subset = R.symbol ~generic:true "["

  let subset2 = R.symbol ~generic:true "[["

  let rle = R.symbol ~generic:true "rle"

  let sample = R.symbol ~generic:true "sample"
end

let length l = R.eval Symbol.length [ R.arg id l ]

let dim x =
  R.eval Symbol.dim [ R.arg id x ]

let subset x i = R.eval Symbol.subset [
    R.arg id x ;
    R.arg id i ;
  ]

let subset_ii x i j = R.eval Symbol.subset [
    R.arg id x ;
    R.arg id i ;
    R.arg id j ;
  ]


let subset2_i x i = R.eval Symbol.subset2 [
    R.arg id x  ;
    R.arg id i
  ]

let subset2_s = subset2_i


let rle x = R.eval Symbol.rle [ R.arg id x ]

let sample x n ?replace ?prob () =
  R.eval Symbol.sample [
    R.arg id x ;
    R.arg id n ;
    R.opt id "replace" replace ;
    R.opt id "prob" prob
  ]
