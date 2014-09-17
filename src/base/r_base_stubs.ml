let id x = x

module Symbol = struct
  let length = R.symbol ~generic:true "length"

  let subset2 = R.symbol ~generic:true "[["

  let rle = R.symbol ~generic:true "rle"
end

let length l = R.eval Symbol.length [ R.arg id l ]


let subset2_i x i = R.eval Symbol.subset2 [
  R.arg id x  ;
  R.arg id i
]

let subset2_s = subset2_i


let rle x = R.eval Symbol.rle [ R.arg id x ]


