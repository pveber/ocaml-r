open OCamlR

module Stubs = OCamlR_base_stubs
module Stubs2 = OCamlR_base_stubs2

let subset2_symbol = symbol ~generic:true "[["

let gen_raw_subset2 label_dec x label =
  call subset2_symbol [
      arg Enc.sexp x ;
      arg label_dec label ;
  ]

let raw_subset2 = gen_raw_subset2 Enc.string
let raw_subset2_i = gen_raw_subset2 Enc.int

module Environment = struct
  include Envsxp

  let create () =
    Stubs.new'env ()
    |> unsafe_of_sexp

  let get env ~class_ x =
    let y = raw_subset2 (to_sexp env) x in
    let cls = Sexp._class_ y in
    if List.mem class_ cls then Some y
    else None
end

module Numeric = Realsxp
module Logical = Lglsxp
module Integer = Intsxp
module Character = Strsxp

module Factor = struct
  include Integer

  let factor_fun = symbol "factor"
  let of_integer xs =
    call factor_fun [ arg Integer.to_sexp xs ]
    |> unsafe_of_sexp
  let of_character xs =
    call factor_fun [ arg Character.to_sexp xs ]
    |> unsafe_of_sexp

  let of_array xs = of_integer (of_array xs)
  let of_list xs = of_integer (of_list xs)
  let of_array_opt xs = of_integer (of_array_opt xs)

  let levels x =
    attr x "levels"
    |> Character.unsafe_of_sexp
end

module List_ = struct
  include Vecsxp

  let as_vecsxp x = x

  let gen_subset2 subset2 x field dec =
    subset2 (to_sexp x) field
    |> Sexp.nil_map ~f:dec

  let subset2 x field dec = gen_subset2 raw_subset2 x field dec
  let subset2_i x field dec = gen_subset2 raw_subset2_i x field dec

  let gen_subset2_exn f label x field dec =
    match f x field dec with
    | None -> failwith label
    | Some y -> y

  let subset2_exn x field dec = gen_subset2_exn subset2 "subset2_exn" x field dec
  let subset2_i_exn x field dec = gen_subset2_exn subset2_i "subset2_i_exn" x field dec
end

module Dataframe = struct
  include List_
  let as_list x = x

  let dim x =
    match Stubs.dim'data'frame ~x:(to_sexp x) () |> Dec.ints with
    | [| i ; j |] -> (i, j)
    | _ -> assert false

  let of_env (env : Environment.t) x =
    Environment.get env ~class_:"data.frame" x
    |> Option.map unsafe_of_sexp

  type column_data =
    | Numeric of Numeric.t
    | Logical of Logical.t
    | Character of Character.t
    | Factor of Factor.t
    | Integer of Integer.t

  type column = string * column_data

  let rarg_of_column_data name =
    let f g x = arg g ~name x in
    function
    | Numeric x -> f Numeric.to_sexp x
    | Logical x -> f Logical.to_sexp x
    | Character x -> f Character.to_sexp x
    | Integer x -> f Integer.to_sexp x
    | Factor x -> f Factor.to_sexp x

  let numeric name x = name, Numeric x
  let integer name x = name, Integer x
  let logical name x = name, Logical x
  let character name x = name, Character  x
  let factor name x = name, Factor x


  let create cols =
    List.map
      (fun (label, col) -> rarg_of_column_data label col)
      cols
    |> call (symbol "data.frame")
    |> unsafe_of_sexp

  let rbind x y =
    call Stubs.rbind_symbol [
      arg to_sexp x ;
      arg to_sexp y
    ]
    |> unsafe_of_sexp

  let cbind x y =
    call Stubs.cbind_symbol [
      arg to_sexp x ;
      arg to_sexp y ;
    ]
    |> unsafe_of_sexp
end

module Matrix = struct
  include Numeric

  let dim (x : t) =
    match Stubs2.dim (to_sexp x) |> Dec.ints with
    | [| i ; j |] -> (i, j)
    | _ -> assert false

  let of_arrays m =
    let data =
      Array.to_list m
      |> Array.concat
      |> Enc.floats
    in
    Stubs.matrix ~data ~nrow:(Enc.int (Array.length m)) ~byrow:(Enc.bool true) ()
    |> unsafe_of_sexp
end

let sample ?replace ?prob ~size x =
  Stubs.sample
    ~x:(Enc.floats x)
    ~size:(Enc.int size)
    ?replace:(Option.map Enc.bool replace)
    ?prob:(Option.map Enc.floats prob)
    ()
  |> Dec.floats

let readRDS fn =
  Stubs.readRDS ~file:(Enc.string fn) ()

let saveRDS ?ascii ?compress ~file obj =
  Stubs.saveRDS
    ~object_:obj
    ~file:(Enc.string file)
    ?ascii:(Option.map Enc.bool ascii)
    ?compress:(Option.map Enc.bool compress)
    ()
  |> ignore
