module M = OCamlR

let _ =
  try
    ignore (R.eval_string "azer")
  with
    R.Runtime_error (x,y) -> (
      print_endline y ;
      let r = Obj.repr y in
      if Obj.is_block r then print_int (Obj.tag r)
    )
;;


















