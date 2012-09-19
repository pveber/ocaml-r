type t = statement list

and statement = 
  | St_expr of expr
  | St_assign of lvalue * expr
      
and expr = 
  | Expr_int of int
  | Expr_id of string
  | Expr_apply of expr * arg list

and lvalue =
  | Lval_id of string

and arg = 
  | Arg_anon of expr
  | Arg_named of string * expr




















