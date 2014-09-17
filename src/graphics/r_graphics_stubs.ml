let () = ignore (R.eval_string "require(graphics, quietly=TRUE)")

let id x = x

module Symbol = struct

  let plot = R.symbol ~generic:true "plot"

end

let plot ?main ?xlab ?ylab ?xlim ?ylim x =
  R.eval Symbol.plot [
    R.arg id x ;
    R.opt id "main" main ;
    R.opt id "xlab" xlab ;
    R.opt id "ylab" ylab ;
    R.opt id "xlim" xlim ;
    R.opt id "ylim" ylim ;
  ]

let plot2 ?main ?xlab ?ylab ?xlim ?ylim x y =
  R.eval Symbol.plot [
    R.arg id x ;
    R.arg id y ;
    R.opt id "main" main ;
    R.opt id "xlab" xlab ;
    R.opt id "ylab" ylab ;
    R.opt id "xlim" xlim ;
    R.opt id "ylim" ylim ;
  ]
