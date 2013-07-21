module M = OCamlR

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


















