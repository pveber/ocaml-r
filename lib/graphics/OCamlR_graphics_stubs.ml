open OCamlR

let () = ignore (R.Eval.string "require(graphics, quietly=TRUE)")

let id x = x

module Symbol = struct

  let plot = R.symbol ~generic:true "plot"
  let par = R.symbol "par"

end

let plot ?main ?xlab ?ylab ?xlim ?ylim ?y x =
  let open R.Eval in
  call Symbol.plot [
    arg id x ;
    opt_arg id "y" y ;
    opt_arg id "main" main ;
    opt_arg id "xlab" xlab ;
    opt_arg id "ylab" ylab ;
    opt_arg id "xlim" xlim ;
    opt_arg id "ylim" ylim ;
  ]

let plot2
    ?main ?xlab ?ylab ?xlim ?ylim x y =
  let open R.Eval in
  call Symbol.plot [
    arg id x ;
    arg id y ;
    opt_arg id "main" main ;
    opt_arg id "xlab" xlab ;
    opt_arg id "ylab" ylab ;
    opt_arg id "xlim" xlim ;
    opt_arg id "ylim" ylim ;
  ]

let par ?mfrow () =
  let open R.Eval in
  call Symbol.par [
    opt_arg id "mfrow" mfrow ;
  ]
