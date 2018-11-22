(* #camlp4o;; *)
(* #require "R.syntax";; *)
(* #require "R.stats";; *)

open OCamlR_base

let res =
  OCamlR_stats.fisher'test 
    (Logical.of_array [| true ; true ; false ; false ; true ; false|])
    (Logical.of_array [| true ; true ; true ; false ; true ; false|])

let () =
  Printf.printf
    "%f %f\n%s\n"
    res#estimate
    res#p'value
    res#alternative
;;
