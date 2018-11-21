open Base
open OCamlR

open Printf


let keywords = [
  "method" ;
  "object" ;
  "class" ;
  "fun" ;
  "function" ;
  "new" ;
  "type" ;
  "module" ;
  "struct" ;
  "sig" ;
  "to" ;
  "done" ;
  "while" ;
  "match" ;
  "with" ;
  "try" ;
  "open" ;
  "begin" ;
  "end" ;
  "or" ;
  "and" ;
  "mod" ;
  "lsl" ;
  "lsr" ;
  "val" ;
]

let ocamlify n =
  let n = String.substr_replace_all n ~pattern:"<-" ~with_:"__" in
  let n = if Char.(n.[0] = '.')
    then String.mapi n ~f:(fun i c -> if i = 0 then '_' else c)
    else n in
  let n = String.tr ~target:'.' ~replacement:'\'' n in
  if Char.is_uppercase n.[0] then "_" ^ n
  else (
    if List.mem ~equal:Caml.( = ) keywords n then n ^ "_"
    else n
  )

let filter_names names =
  Array.filter names ~f:(function
      | "" -> false
      | n -> (
          match n.[0] with
          | 'a'..'z' | 'A'..'Z' | '_' -> true
          | _ -> false
        )
    )

let transform_arg_name_list l =
  List.filter l ~f:String.(fun x -> x <> "...")

let generate_stub_ml name str =
  let open R.Pretty in
  match str with
  | CLOSURE { formals ; _ } ->
    let args = match formals with
      | LIST args -> args
      | NULL -> []
      | _ -> failwith "closure arguments not supported"
    in
    let name_of_arg = function
      | ARG name, _ -> name
      | SYMBOL (Some (name,_)), _ -> name
      | _ -> failwith ("arg: " ^ name)
    in
    let param_of_arg x = sprintf "?%s" (ocamlify x) in
    let r_arg_of_arg x =
      sprintf {|R.opt (fun x -> x) "%s" %s|} x (ocamlify x)
    in
    let args = List.map args ~f:name_of_arg in
    let args = transform_arg_name_list args in
    let ocaml_name = ocamlify name in
    sprintf {|
let %s_symbol = R.symbol "%s"
let %s %s () = R.eval %s_symbol [ %s ]|}
      ocaml_name
      name
      ocaml_name
      (String.concat ~sep:" " (List.map args ~f:param_of_arg))
      ocaml_name
      (String.concat ~sep:" ; " (List.map args ~f:r_arg_of_arg))
  | STRINGS _ ->
    sprintf {|let %s : string array R.t = R.eval_string "%s"|} (ocamlify name) name
  | BOOLS _ ->
    sprintf {|let %s : bool array R.t = R.eval_string "%s"|} (ocamlify name) name
  | FLOATS _ ->
    sprintf {|let %s : float array R.t = R.eval_string "%s"|} (ocamlify name) name
  | VECSXP _ -> "" (* TODO *)
  | BUILTIN
  | SPECIAL _ -> "" (* FIXME: how to handle this case? *)
  | _ -> failwithf "not supported: %s" name ()

let generate_stub_ml_for_package p =
  let () = ignore @@ R.eval_string {|require(utils, quietly=TRUE)|} in
  let () = ignore @@ R.eval_string (sprintf {|require(%s, quietly=TRUE)|} p) in
  let r_list = R.eval_string (sprintf {|ls("package:%s")|} p) in
  let funs = filter_names (R.strings_of_t r_list) in
  Caml.print_endline "open OCamlR" ;
  Caml.print_endline (
    sprintf
      {|let () = ignore (R.eval_string "require(%s, quietly=TRUE)")|}
      p
  ) ;
  Array.iter funs ~f:(fun name ->
        let value = R.symbol name in
        let str = R.Pretty.t_of_sexp (value : _ R.t :> R.sexp) in
        generate_stub_ml name str
        |> Caml.print_endline
    )

let () =
  generate_stub_ml_for_package Caml.Sys.argv.(1)
