(* #camlp4o;; *)
(* #require "R.syntax";; *)
(* #require "R.stats";; *)

open OCamlR_base

module FT = OCamlR_stats.Fisher'test

let res =
  FT.logicals
    (Logical.of_array [| true ; true ; false ; false ; true ; false|])
    (Logical.of_array [| true ; true ; true ; false ; true ; false|])

let () =
  Printf.printf
    "%f %f\n%s\n"
    (FT.estimate res)
    (FT.p'value res)
    (FT.alternative res)
;;
