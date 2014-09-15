let _ =
  try
    ignore (R.eval_string "azer")
  with
    R.Runtime_error (x,y) -> print_string y
;;

let _ =
  try
    ignore (R.eval_string "list()[[1]]")
  with
    R.Runtime_error (x,y) -> print_endline y
;;

let _ =
  let l = [ 1. ; 2. ; 3. ] in
  assert (l = R.floats_of_t (R.floats l))
















