let () = ignore (R.eval_string "require(grDevices, quietly=TRUE)")

module Stub = struct

  let png = R.symbol "png"

  (* TODO: This segfaults: let dev = R.symbol "dev" *)

end
