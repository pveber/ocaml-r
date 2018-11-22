open OCamlR

let () =
  R.eval
    (R.symbol ~generic:true "print")
    [ R.arg (fun x -> x) (R.optints [|None ; None ; Some 42|]) ]
  |> ignore
;;

let _ =
  try
    ignore (R.eval_string "azer")
  with
    R.Runtime_error (_, y) -> print_string y
;;

let _ =
  try
    ignore (R.eval_string "list()[[1]]")
  with
    R.Runtime_error (_, y) -> print_endline y
;;

let _ =
  let l = [| 1. ; 2. ; 3. |] in
  assert (l = R.floats_of_t (R.floats l))
















