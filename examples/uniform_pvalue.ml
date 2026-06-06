module S = OCamlR_stats
module G = OCamlR_graphics
module Gr = OCamlR_grDevices

let xs = Array.init 10_000 (fun _ ->
    let x = S.rnorm 30 in
    S.T'test.(one_sample x |> p'value)
  )

let () =
  Gr.pdf "hist.pdf" ;
  ignore (G.hist xs) ;
  Gr.dev_off ()

