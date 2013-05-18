%{
  open Printf
  open R_lang_ast

  let typ_of_string = function
    | "i" -> `int
    | "s" -> `string
    | "v" -> `vector
    | "r" -> `r
    | x -> failwith (sprintf "Unknown conversion character %s" x)
%}

%token <int> INT
%token <float> FLOAT
%token <string> IDENT
%token <string> STRING
%token <string * Camlp4.PreCast.Syntax.Ast.expr> ANTIQUOT
%token SEMICOLON COMMA DOT LPAREN RPAREN 
%token LBRACKET RBRACKET
%token PLUS MINUS TIMES DIV SHARP
%token LT GT AMPERSAND TILDE
%token EQUAL ASSIGN EOL EOI
%left TILDE
%left AMPERSAND
%left LT GT
%left PLUS MINUS
%left TIMES DIV
%left SHARP
%nonassoc LPAREN
%nonassoc LBRACKET

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
| f = FLOAT
    { Expr_float f }
| id = ident
    { Expr_id id }
| s = STRING
    { Expr_string s }
| a = ANTIQUOT
    { let (k,expr) = a in
      Expr_antiquot (Pa_r.random_var (), typ_of_string k, expr) }
| e = expr LBRACKET indices = separated_nonempty_list(COMMA,option(expr)) RBRACKET
    { Expr_index (e,indices) }
| e = expr LPAREN args = separated_list(COMMA,arg) RPAREN
    { Expr_apply (e,args) }
| LPAREN e = expr RPAREN
    { e }
| e = expr TILDE f = expr
    { Expr_op (e, Op_tilde, f) }
| e = expr AMPERSAND f = expr
    { Expr_op (e, Op_and, f) }
| e = expr GT f = expr
    { Expr_op (e, Op_gt, f) }
| e = expr LT f = expr
    { Expr_op (e, Op_lt, f) }
| e = expr PLUS f = expr
    { Expr_op (e, Op_plus, f) }
| e = expr MINUS f = expr
    { Expr_op (e, Op_minus, f) }
| e = expr TIMES f = expr
    { Expr_op (e, Op_times, f) }
| e = expr DIV f = expr
    { Expr_op (e, Op_div, f) }
| e = expr SHARP id = ident
    { Expr_subset (e, id) }
| MINUS e = expr
    { Expr_unop (Op_minus, e) }
;

arg:
| expr { Arg_anon $1 }
| argname = ident EQUAL expr { Arg_named (argname,$3) }
;

lvalue:
| s = ident
    { Lval_id s }
;

ident:
| id = separated_nonempty_list(DOT,IDENT)
    { String.concat "." id }


















