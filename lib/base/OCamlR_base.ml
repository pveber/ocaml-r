open OCamlR
open OCamlR_wraputils

module Stubs = OCamlR_base_stubs
module Stubs2 = OCamlR_base_stubs2

let ( |? ) o f = match o with
  | Some x -> Some (f x)
  | None -> None

let subset x i = Stubs2.subset x (R.int i)
let subset_ii x i j = Stubs2.subset_ii x (R.int i) (R.int j)
let subset2_i x i = Stubs2.subset2_i x (R.int i)
let subset2_s x s = Stubs2.subset2_s x (R.string s)

module type Atomic_vector = sig
  type t
  type format
  type elt
  val r : t -> format R.t
  val length : t -> int
  val to_array : t -> elt array
  val of_array : elt array -> t
end

module Atomic_vector_impl(X : sig
    type format
    type elt
    val to_array : format R.t -> elt array
    val of_array : elt array -> format R.t
  end) = struct
  include X
  type t = X.format R.t

  let r x = x
  let to_array = X.to_array
  let of_array = X.of_array
  let length =
    let symbol = R.symbol "length" in
    fun (x : t) ->
      R.int_of_t (R.eval symbol [ R.arg ident x ])
end

module Numeric = struct
  module E = struct
    type format = R.reals
    type elt = float
    let to_array = R.floats_of_t
    let of_array = R.floats
  end
  include Atomic_vector_impl(E)
end

module Integer = struct
  module E = struct
    type format = R.integers
    type elt = int
    let to_array = R.ints_of_t
    let of_array = R.ints
  end
  include Atomic_vector_impl(E)
end

module Logical = struct
  module E = struct
    type format = R.logicals
    type elt = bool
    let to_array = R.bools_of_t
    let of_array = R.bools
  end
  include Atomic_vector_impl(E)
end

module Character = struct
  module E = struct
    type format = R.strings
    type elt = string
    let to_array = R.strings_of_t
    let of_array = R.strings
  end
  include Atomic_vector_impl(E)
end

module Factor = struct
  include Character
end

module S3 = struct
  type t = t R.t
  let r x = x
  let _class_ =
    let symbol = R.symbol "class" in
    fun (x : t) ->
      R.strings_of_t (R.eval symbol [ R.arg ident x ])
end

module Environment = struct
  include S3
  let create () = Stubs.new'env ()
  let unsafe_get env ~class_ x =
    let y = Stubs2.subset2_s env (R.string x) in
    let cls = R.classes (y : _ R.t :> R.sexp) in
    if List.mem class_ cls then
      Some y
    else None
end

module Dataframe = struct
  include S3

  let dim x =
    match Stubs.dim'data'frame ~x () |> R.ints_of_t with
    | [| i ; j |] -> (i, j)
    | _ -> assert false

  let of_env env x =
    Environment.unsafe_get env ~class_:"data.frame" x

  type column_data =
    | Numeric of Numeric.t
    | Logical of Logical.t
    | Character of Character.t
    | Factor of Factor.t
    | Integer of Integer.t

  type column = string * column_data

  let rarg_of_column_data name =
    let f x = R.arg (fun x -> x) ~name x in
    function
    | Numeric x -> f x
    | Logical x -> f x
    | Character x -> f x
    | Integer x -> f x
    | Factor x -> f x

  let numeric name x = name, Numeric x
  let integer name x = name, Integer x
  let logical name x = name, Logical x
  let character name x = name, Character  x
  let factor name x = name, Factor x


  let create cols =
    List.map
      (fun (label, col) -> rarg_of_column_data label col)
      cols
    |> R.eval (R.symbol "data.frame")
end

let sample ?replace ?prob ~size x =
  Stubs.sample
    ~x:(R.floats x)
    ~size:(R.int size)
    ?replace:(replace |? R.bool)
    ?prob:(prob |? R.floats)
    ()
  |> R.floats_of_t


(* class data'frame x = object *)
(*   inherit list_ x *)
(*   method dim = dim x *)
(*   method subset_ii : 'b. int -> int -> 'b R.t = *)
(*     subset_ii x *)
(* end *)

(* module Stub = struct *)

(*   (\*   Information about the content of R standard library: *)
(*     *  http://stat.ethz.ch/R-manual/R-patched/doc/html/packages.html *\) *)

(*   (\*   Information about the R base package: *)
(*     *  http://stat.ethz.ch/R-manual/R-patched/library/base/html/00Index.html *\) *)

(*   let sample = R.symbol "sample" *)

(*   let lapply = R.symbol "lapply" *)

(*   let tilde = R.symbol "~" *)

(*   let dollar = R.symbol "$" *)

(*   let dot_subset2 = R.symbol ".subset2" *)

(*   let t = R.symbol "t" *)

(*   let cbind = R.symbol "cbind" *)

(*   let rbind = R.symbol "rbind" *)

(*   let matrix = R.symbol "matrix" *)


(* end *)

(* let lapply (x : 'a list R.t) (func : 'b R.t) : 'c list R.t = *)
(*   (\* It would be nice to solve once and for all the typing of *)
(*      R.t values by using the 'private' keyword to access the *)
(*      underlying R.sexp value by subtyping, and by using *)
(*      polymorphic variants for the parametrised typing of R.t. *\) *)
(*   (\* There is a ... in the args of lapply, for params passed *)
(*      to the function 'func'. Might be intelligent to wrap it up. *\) *)
(*   R.eval Stub.lapply [ *)
(*     (R.arg (fun x -> x) x)    ; *)
(*     (R.arg (fun x -> x) func) ] *)

(* class array_ r = object (self) *)
(*   inherit R.s3 r *)
(*   method dim : float list R.t =  *)
(*     R.eval Stub.dim [ *)
(*       R.arg (fun x -> x) r *)
(*     ] *)
(* end *)

(* class matrix r = object (self) *)
(*   inherit array_ r *)
(*   method floats : float array array = assert false *)
(* end *)

(* let matrix ?byrow ~nrow ~ncol v =  *)
(*   R.eval Stub.matrix [ *)
(*     R.arg R.floats v ; *)
(*     R.arg R.int            nrow ; *)
(*     R.arg R.int            ncol ; *)
(*     R.opt R.bool   "byrow" byrow ; *)
(*   ] *)

(* let matrix_by_rows = function *)
(* | [] -> matrix ~nrow:0 ~ncol:0 [] *)
(* | h :: t as data ->  *)
(*     let ncol = List.length h in *)
(*     let () =  *)
(*       if List.exists (fun r -> List.length r <> ncol) data *)
(*       then raise (Invalid_argument "Rbase.matrix_by_rows: not all lines have the same dimension") *)
(*     in *)
(*     let nrow = List.length data in *)
(*     matrix ~byrow:true ~nrow ~ncol (List.concat data) *)

(* let tilde (x : 'a R.t) (y : 'a R.t) : 'c R.t = *)
(*   R.eval Stub.tilde [ *)
(*     (R.arg (fun x -> x) x)    ; *)
(*     (R.arg (fun x -> x) y)    ] *)


(* let component (x : 'a R.t) (y : string) : 'c R.t = *)
(*   R.eval Stub.dollar [ *)
(*     (R.arg (fun x -> x) x)    ; *)
(*     (R.arg R.string     y)    ] *)

(* let dot_subset2 l i = *)
(*   R.eval Stub.dot_subset2 [ R.arg (fun x -> x) l ;  *)
(*                             R.arg R.int i ] *)



(* (\* let listing r = new listing r *\) *)

(* (\* let subset2 = R.symbol ~generic: true ".subset2" *\) *)

(* (\* class ['a] dataframe r = object (self) *\) *)
(* (\*   inherit ['a] listing r *\) *)
(* (\*   method row_names = R.strings_of_t (self#attribute "row.names") *\) *)
(* (\*   method column : 'a. int -> 'a R.t = fun x -> R.eval subset2 [ *\) *)
(* (\*     R.arg (fun x -> x) (R.cast __underlying)  ; *\) *)
(* (\*     R.arg R.int        x           ] *\) *)
(* (\*   method element : 'a. int -> int -> 'a R.t = fun x y -> R.eval subset2 [ *\) *)
(* (\*     R.arg (fun x -> x) (R.cast __underlying)     ; *\) *)
(* (\*     R.arg R.int        x              ; *\) *)
(* (\*     R.arg R.int        y              ] *\) *)
(* (\* end *\) *)

(* (\* let dataframe r = new dataframe r *\) *)

(* (\* class date r = object (self) *\) *)
(* (\*   inherit R.s3 r *\) *)
(* (\*   method as_float = R.float_of_t (Obj.magic __underlying) *\) *)
(* (\*   method as_date = CalendarLib.Calendar.Date.from_unixfloat (86400. *. self#as_float) *\) *)
(* (\* end *\) *)





(* let to_list (listing : 'a list #listing R.t) = *)
(*   List.map *)
(*     R.cast *)
(*     (R.sexps_of_t (R.cast (listing : 'c R.t :> R.sexp) : R.sexp list R.t)) *)
