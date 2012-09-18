open Camlp4.PreCast

let script_expander _loc _ s = <:expr< 0 >>

let () = Quotation.(add "rscript" Quotation.DynAst.expr_tag) script_expander










