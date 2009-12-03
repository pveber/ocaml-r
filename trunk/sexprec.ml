external sexp_equality : sexp -> sexp -> bool = "r_sexp_equality"

(* R constants - global symbols in libR.so. *)
(* We are looking for a clean solution
   for the typing of the R NULL. What should it be
   in OCaml? An 'a option mapping to None? *)
external null_creator : unit -> nil sxp = "r_null"
external dots_symbol_creator : unit -> sexp = "r_dots_symbol"
external missing_arg_creator : unit -> sexp = "r_missing_arg"
external base_env_creator : unit -> sexp = "r_base_env"

(* R_GlobalEnv is not a constant, but rather a constant pointer,
   that gets updated by R itself. *)
external global_env : unit -> sexp = "r_global_env"