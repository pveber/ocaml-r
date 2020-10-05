open OCamlR
open OCamlR.Eval

let id x = x

module Symbol = struct
  let length = symbol ~generic:true "length"

  let dim = symbol ~generic:true "dim"

  let subset = symbol ~generic:true "["

  let subset2 = symbol ~generic:true "[["

  let rle = symbol ~generic:true "rle"

  let sample = symbol ~generic:true "sample"

  let min = symbol ~generic:true "min"

  let max = symbol ~generic:true "max"
end

let length l = Eval.call Symbol.length [ arg id l ]

let dim x =
  Eval.call Symbol.dim [ arg id x ]

let subset x i = Eval.call Symbol.subset [
    arg id x ;
    arg id i ;
  ]

let subset_ii x i j = Eval.call Symbol.subset [
    arg id x ;
    arg id i ;
    arg id j ;
  ]


let subset2_i x i = Eval.call Symbol.subset2 [
    arg id x  ;
    arg id i
  ]

let subset2_s = subset2_i

module Matrix = struct
  let subset = subset
  let subset_ii = subset_ii
  let subset2 = subset2_i
end

let rle x = Eval.call Symbol.rle [ arg id x ]

let sample x n ?replace ?prob () =
  call Symbol.sample [
    arg id x ;
    arg id n ;
    opt_arg id "replace" replace ;
    opt_arg id "prob" prob
  ]

let min x = Eval.call Symbol.min [ arg id x ]
let max x = Eval.call Symbol.max [ arg id x ]

