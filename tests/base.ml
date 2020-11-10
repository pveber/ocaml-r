open OCamlR_base

module Testable_numeric = struct
  include Numeric
  let equal x y =
    to_array x = to_array y
  let pp = Fmt.(using to_array (array float))
end

let numeric : Numeric.t Alcotest.testable = (module Testable_numeric)

let test_c () =
  Alcotest.check numeric "c"
    Numeric.(of_array [|1.;2.;3.;4.;5.;6.|])
    Numeric.(c [of_array [|1.;2.;3.|] ; of_array [|4.;5.;6.|]])

let () =
  let open Alcotest in
  run "base" [
    "numeric", [
      test_case "c" `Quick test_c ;
    ] ;
  ]
