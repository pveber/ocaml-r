%{
  open Printf
  open R_lang_ast

  let typ_of_string = function
    | "i" -> `int
    | "r" -> `r
    | x -> failwith (sprintf "Unknown conversion character %s" x)
%}

%token <int> INT
%token <string> IDENT
%token <string * Camlp4.PreCast.Syntax.Ast.expr> ANTIQUOT
%token SEMICOLON COMMA LPAREN RPAREN
%token EQUAL ASSIGN EOL EOI

%start prog
%type <R_lang_ast.t> prog

%%

prog:
| statements EOI { List.rev $1 }
;

statements:
| 
    { [] }
| statements EOL { $1 }
| statements statement { $2 :: $1 }
;

statement:
| expr eos               { St_expr $1 }
| lvalue ASSIGN expr eos { St_assign ($1,$3) }
;

eos:
| SEMICOLON { () }
| EOL { () }
;

expr:
| i = INT
    { Expr_int i }
| s = IDENT
    { Expr_id s }
| a = ANTIQUOT
    { let (k,expr) = a in
      Expr_antiquot (Pa_r.random_var (), typ_of_string k, expr) }
| e = expr LPAREN args = separated_list(COMMA,arg) RPAREN
    { Expr_apply (e,args) }
;

arg:
| expr { Arg_anon $1 }
| argname = IDENT EQUAL expr { Arg_named (argname,$3) }
;

lvalue:
| s = IDENT
    { Lval_id s }
;




















