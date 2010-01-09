(* The following exception needs to be registered
   in a callback when the R interpreter is initialised. *)
exception R_Error of lang sxp * string

external eval_langsxp : lang sxp -> sexp = "r_eval_sxp"

let eval_string s = eval_langsxp (parse_sexp s)

let rec prepare_args = function
  | (Some x)::l -> x::(prepare_args l)
  | None::l     -> prepare_args l
  | []          -> []

let arg f ?name x = Some (name, (sexp (f x)))
let opt f name x = match x with
  | None -> None
  | Some xx -> Some ((Some name), (sexp (f xx)))

let eval phi (args: (string option * sexp) option list) =
  eval_langsxp (langsxp phi (prepare_args args))
