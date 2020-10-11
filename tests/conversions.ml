open OCamlR
open OCamlR_base

let test_inherits () =
  Alcotest.(check bool) "inherits" true (inherits (eval_string "c(1)") "numeric")

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

let test_realsxp_get () =
  test_back_and_from Alcotest.(array (float 0.000001)) Realsxp.of_array (fun vec ->
      let n = Realsxp.length vec in
      Array.init n (Realsxp.get vec)
    )
    [
      [| 3.14 ; -12312312312.1 |] ;
    ]

let test_realsxp_get_opt () =
  test_back_and_from Alcotest.(array (option (float 0.000001))) Realsxp.of_array_opt (fun vec ->
      let n = Realsxp.length vec in
      Array.init n (Realsxp.get_opt vec)
    )
    [
      [| Some 3.14 ; None ; Some (-12312312312.1) |] ;
    ]

let test_matrix () =
  test_back_and_from Alcotest.(array (array (float 0.000001))) Numeric.Matrix.of_arrays (fun mat ->
      let nr, nc = Numeric.Matrix.dim mat in
      Array.init nr (fun i ->
          Array.init nc (Numeric.Matrix.get2 mat i)
        )
    )
    [
      [| [| 1. ; 2. |] ; [| 3. ; 4. |] |] ;
    ]

let test_matrix_slice () =
  let test label f m k ~expected =
    Alcotest.(check' (array (float 0.000001)))
      ~msg:label ~expected
      ~actual:(f (Numeric.Matrix.of_arrays m) k |> Numeric.to_array)
  in
  let m = [| [| 1. ; 2. |] ; [| 3. ; 4. |] |] in
  Numeric.Matrix.print (Numeric.Matrix.of_arrays m) ;
  test "get_row" Numeric.Matrix.get_row m 2 ~expected:[| 3. ; 4. |] ;
  test "get_col" Numeric.Matrix.get_col m 1 ~expected:[| 1. ; 3. |]

let test_factor () =
  Alcotest.(check (list string)) "levels" ["a";"b";"c"] (
      Character.of_list ["b";"a";"a";"a";"c"]
      |> Factor.of_character
      |> Factor.levels
      |> Character.to_list
  )

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
      test_case "of_list, to_list" `Quick test_intsxp ;
      test_case "of_array_opt, to_array_opt" `Quick test_intsxp_opt ;
    ] ;
    "strsxp", [
      test_case "of_list, to_list" `Quick test_strsxp ;
      test_case "of_array_opt, to_array_opt" `Quick test_strsxp_opt ;
    ] ;
    "realsxp", [
      test_case "of_array_opt, to_array_opt" `Quick test_realsxp_get_opt ;
      test_case "get" `Quick test_realsxp_get ;
    ] ;
    "vecsxp", [ test_case "of_list, to_list" `Quick test_vecsxp ] ;
    "factor", [ test_case "of_character, levels" `Quick test_factor ] ;
    "matrix", [
      test_case "dim, subset" `Quick test_matrix ;
      test_case "get_row, get_col" `Quick test_matrix_slice ;
    ] ;
  ]
