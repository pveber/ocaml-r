open OCamlR.R

let test_back_and_from ty to_r from_r cases =
  let f i case =
    (to_r case |> from_r)
    |> Alcotest.(check ty) (string_of_int i) case 
  in
  List.iteri f cases

let test_intsxp () =
  test_back_and_from Alcotest.(list int) Intsxp.of_list Intsxp.to_list [
    [ 1 ; 2 ; -10 ] ;
    [] ;
    List.init 1_000 (fun i -> - i) ;
  ]

let test_intsxp_opt () =
  test_back_and_from Alcotest.(array (option int)) Intsxp.of_array_opt Intsxp.to_array_opt
    [
      [| Some 1 ; None ; Some (-10) |] ;
      [||] ;
      Array.init 1_000 (fun _ -> None) ;
    ]

let test_strsxp () =
  test_back_and_from Alcotest.(list string) Strsxp.of_list Strsxp.to_list [
    [ "1" ; "2" ; "-10" ; "" ] ;
    [] ;
    List.init 1_000 string_of_int ;
  ]

let test_strsxp_opt () =
  test_back_and_from Alcotest.(array (option string)) Strsxp.of_array_opt Strsxp.to_array_opt
    [
      [| Some "1" ; None ; Some "-10" ; Some "" |] ;
      [||] ;
      Array.init 1_000 (fun _ -> None) ;
    ]

let test_vecsxp () =
  test_back_and_from Alcotest.(array (list string))
    (fun xs ->
       Array.map Strsxp.(fun x -> of_list x |> to_sexp) xs
       |> Vecsxp.of_array)
    (fun l ->
       Vecsxp.to_array l
       |> Array.map Strsxp.(fun x -> to_list (unsafe_of_sexp x)))
    [
      [| [] ; ["a"] ; ["a" ; "b"] |] ;
      [| |] ;
    ]

let () =
  let open Alcotest in
  run "Conversions" [
    "intsxp", [
      test_case "Intsxp" `Quick test_intsxp ;
      test_case "Intsxp with NAs" `Quick test_intsxp_opt ;
    ] ;
    "strsxp", [
      test_case "Strsxp" `Quick test_strsxp ;
      test_case "Strsxp with NAs" `Quick test_strsxp_opt ;
    ] ;
    "vecsxp", [ test_case "Vecsxp" `Quick test_vecsxp ] ;
  ]

