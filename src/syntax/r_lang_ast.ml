open Printf

type t = statement list

and statement = 
  | St_expr of expr
  | St_assign of lvalue * expr
      
and expr = 
  | Expr_int of int
  | Expr_id of string
  | Expr_string of string
  | Expr_apply of expr * arg list
  | Expr_antiquot of string * [`r | `int | `string | `vector] * Camlp4.PreCast.Syntax.Ast.expr

and lvalue =
  | Lval_id of string

and arg = 
  | Arg_anon of expr
  | Arg_named of string * expr

let lvalue_to_string = function
| Lval_id id -> id

let rec expr_to_string = function
| Expr_id id -> id
| Expr_string s -> sprintf "'%s'" s
| Expr_int i -> string_of_int i
| Expr_apply (e,args) ->
    sprintf "(%s)(%s)"
      (expr_to_string e)
      (String.concat "," (List.map arg_to_string args))
| Expr_antiquot (var,_,_) -> var

and arg_to_string = function
| Arg_anon e -> expr_to_string e
| Arg_named (arg_id, e) ->
    arg_id ^ " = " ^ (expr_to_string e)

let statement_to_string = function
| St_expr e -> (expr_to_string e) ^ "\n"
| St_assign (lvalue, e) ->
    (lvalue_to_string lvalue) ^ " <- " ^ (expr_to_string e) ^ "\n"

let to_string prog = 
  String.concat "" (List.map statement_to_string prog)

let rec free_variables prog = 
  List.fold_left
    (fun accu st -> (free_variables_of_statement st) @ accu)
    [] prog

and free_variables_of_statement = function
  | St_expr e -> free_variables_of_expr e
  | St_assign (_,e) -> free_variables_of_expr e

and free_variables_of_expr = function
  | Expr_antiquot (var,typ,e) -> [ (var, typ, e) ]
  | Expr_apply (fun_expr, args) ->
      (free_variables_of_expr fun_expr) @ (List.fold_left (fun accu x -> (free_variables_of_arg x) @ accu) [] args)
  | _ -> []

and free_variables_of_arg = function
| Arg_anon e | Arg_named (_,e) -> free_variables_of_expr e





















