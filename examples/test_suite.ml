open OCamlR

let () =
  call
    (symbol ~generic:true "print")
    [ arg Enc.int_opts [|None ; None ; Some 42|] ]
  |> ignore
;;

let _ =
  try
    ignore (eval_string "azer")
  with
    Runtime_error (_, y) -> print_string y
;;

let _ =
  try
    ignore (eval_string "list()[[1]]")
  with
    Runtime_error (_, y) -> print_endline y
;;

let _ =
  let l = [| 1. ; 2. ; 3. |] in
  assert (l = Dec.floats (Enc.floats l))
















