open Rbase

let res : < estimate : float R.t > listing R.t = R.eval_string "fisher.test(matrix(c(2,3,4,5),ncol=2))";;

let () = 
  print_float (R.float_of_t ((listing res)#component "p.value")) ;
  print_newline ()
;;

let () = 
  print_float (R.float_of_t ((listing res) ## estimate)) ;
  print_newline ()
;;

let () = 
  print_float (R.float_of_t ((listing res) ## mestimate)) ;
  print_newline ()
;;
