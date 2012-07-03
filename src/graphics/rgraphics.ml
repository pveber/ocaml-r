let () = ignore (R.eval_string "require(graphics, quietly=TRUE)")

module Stub = struct

  let hist = R.symbol "hist"

end

let r_breaks = function
| `n n -> Obj.magic (R.int n)
| `l v -> Obj.magic v
| `m `Sturges -> Obj.magic (R.string "Sturges")
| `m `Scott -> Obj.magic (R.string "Scott")
| `m `FD -> Obj.magic (R.string "FD")

let hist ?breaks ?freq ?include_lowest ?right ?main ?xlab ?ylab ?xlim ?ylim x = 
  R.eval Stub.hist [
    R.arg (fun x -> x)                  x ;
    R.opt r_breaks     "breaks"         breaks ;
    R.opt R.bool       "freq"           freq ;
    R.opt R.bool       "include_lowest" include_lowest ;
    R.opt R.bool       "right"          right ;
    R.opt R.string     "main"           main;
    R.opt R.string     "xlab"           xlab ;
    R.opt R.string     "ylab"           ylab ;
    R.opt R.float      "xlim"           xlim ;
    R.opt R.float      "ylim"           ylim ;
  ]




















