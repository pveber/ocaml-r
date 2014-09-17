let id x = x

module Symbol = struct
  let rnorm = R.symbol ~generic:true "rnorm"
end

let rnorm ?mean ?sd n =
  R.eval Symbol.rnorm [
    R.arg id n ;
    R.opt id "mean" mean ;
    R.opt id "sd" sd
  ]
