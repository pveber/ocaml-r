(* #camlp4o;; *)
(* #require "R.syntax";; *)
(* #require "R.stats";; *)

open Rbase

let res = Rstats.fisher_test_2x2 ~ff:2 ~ft:3 ~tf:4 ~tt:5 ()

let () = 
  Printf.printf 
    "%f %f\n%s\n"
    (R.float_of_t (res ## estimatee))
    (R.float_of_t (res ## p'value))
    (R.string_of_t (res ## alternative))
;;

let _ = (R.int (res ## p'value)) = 0













