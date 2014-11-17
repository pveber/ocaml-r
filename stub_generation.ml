#use "topfind"

#require "R.stats";;
#require "R.graphics";;
#require "core_kernel"

open Core_kernel.Std

let value = R.eval_string "sample"

let str = R.Pretty.t_of_sexp (value : _ R.t :> R.sexp)

let keywords = [ "method" ; "object" ; "class" ; "fun" ; "function" ; "new" ; "type" ; "module" ; "struct" ; "sig" ; "to" ; "done" ; "while" ; "match" ; "with" ; "try" ; "open" ; "begin" ; "end" ; "or" ; "and" ; "mod" ; "lsl" ; "lsr" ]

let ocamlify n =
  let n = String.substr_replace_all n ~pattern:"<-" ~with_:"__" in
  let n = if n.[0] = '.' then (let n = String.copy n in n.[0] <- '_' ; n) else n in
  let n = String.tr ~target:'.' ~replacement:'\'' n in
  if Char.is_uppercase n.[0] then "_" ^ n
  else (
    if List.mem keywords n then n ^ "_"
    else n
  )

let transform_arg_name_list = function
  | "..." :: _ -> failwith ""
  | l ->
    if List.mem l "..." then (
      if List.last_exn l = "..." then List.slice l 0 (-1)
      else failwith ""
    )
    else l

let generate_stub_ml name str =
  let open R.Pretty in
  match str with
  | CLOSURE { formals = LIST args } ->
    let name_of_arg = function
      | ARG name, _ -> name
      | SYMBOL (Some (name,_)), _ -> name
      | _ -> failwith ""
    in
    let param_of_arg x = sprintf "~%s" (ocamlify x) in
    let r_arg_of_arg x =
      sprintf "R.arg (fun x -> x) ~name:\"%s\" %s" x (ocamlify x)
    in
    let args = List.map args ~f:name_of_arg in
    let args = transform_arg_name_list args in
    sprintf "let %s %s = R.eval (R.symbol \"%s\") [ %s ]\n"
      (ocamlify name)
      (String.concat ~sep:" " (List.map args ~f:param_of_arg))
      name
      (String.concat ~sep:" ; " (List.map args ~f:r_arg_of_arg))
  | _ -> failwith ""

let generate_stub_ml_for_package p =
  let r_list = R.eval_string (sprintf "ls(package:%s)" p) in
  let funs = R.strings_of_t r_list in
  List.iter funs ~f:(fun name ->
      try
        let value = R.eval_string name in
        let str = R.Pretty.t_of_sexp (value : _ R.t :> R.sexp) in
        generate_stub_ml name str
        |> print_endline
      with _ -> ()
    )

let () =
  generate_stub_ml_for_package "stats"
