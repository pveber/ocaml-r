let ( |? ) o f = match o with
  | Some x -> Some (f x)
  | None -> None

let rle_gen encoder decoder l =
  R_base_stubs.rle (encoder l)
  |> fun o -> R.ints_of_t (o ## lengths), decoder (o ## values)

let rle_floats l = rle_gen R.floats R.floats_of_t l
let rle_ints l = rle_gen R.ints R.ints_of_t l
let rle_strings l = rle_gen R.strings R.strings_of_t l
let rle_bools l = rle_gen R.bools R.bools_of_t l


(* class t (x : _ R.t) = object (s) *)
(*   method sexp = (x : _ R.t :> R.sexp) *)
(*   method attributes = R.attributes s#sexp *)
(* end *)
(* let t x = new t x *)


(* let subset_sym = R.symbol ~generic:true "[" *)

(* let subset x i = R.eval subset_sym [ *)
(*   R.arg id x ; *)
(*   R.arg R.int i ; *)
(* ] *)

(* class atomic_vector x = object *)
(*   inherit t x *)
(*   method length = length x *)
(* end *)


(* class reals x = object (s) *)
(*   inherit atomic_vector x *)
(*   method get i = R.float_of_t (subset x i) *)
(*   method to_floats = R.floats_of_t (R.cast s#sexp) *)
(* end *)
(* let reals x = new reals x *)

(* class real x = object (s) *)
(*   inherit reals x *)
(*   method to_float = R.float_of_t (R.cast s#sexp) *)
(* end *)
(* let real x = new real x *)

(* class s3 x = object (s) *)
(*   inherit t x *)
(*   method classes = R.classes s#sexp *)
(* end *)

(* let subset2_sym = R.symbol ~generic:true "[[" *)

(* let subset2_i x i = R.eval subset2_sym [ *)
(*   R.arg id x  ; *)
(*   R.arg R.int        i *)
(* ] *)

(* let subset2_s x label = R.eval subset2_sym [ *)
(*   R.arg id x  ; *)
(*   R.arg R.string label *)
(* ] *)


(* class list_ x = object *)
(*   inherit s3 x *)
(*   method length = length x *)
(*   method get : 'a. int -> 'a R.t = *)
(*     fun i -> subset2_i x i *)
(*   method get_s : 'a. string -> 'a R.t = *)
(*     fun s -> subset2_s x s *)
(* end *)

(* let dim_sym = R.symbol "dim" *)

(* let dim df = *)
(*   R.eval dim_sym [ R.arg id df ] *)
(*   |> R.ints_of_t *)
(*   |> (function *)
(*       | [ x ; y ] -> x, y *)
(*       | _ -> assert false) *)


(* let subset_ii x i j = R.eval subset_sym [ *)
(*   R.arg id x ; *)
(*   R.arg R.int i ; *)
(*   R.arg R.int j ; *)
(* ] *)

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

(* let sample (x : 'a list R.t) size ?replace ?(prob: float list option) () : 'a list R.t = *)
(*   (\* Note that size may be left out in this R function. This *)
(*      behaviour does not fit the R.arg behaviour, nor does it *)
(*      fit the R.opt behaviour (since the argument should be *)
(*      named. This type of argument has to be worked out... *\) *)
(*   R.eval Stub.sample [ *)
(*     (R.arg (fun x -> x)             x)      ; *)
(*     (R.arg R.int                    size)   ; *)
(*     (R.opt R.bool         "replace" replace); *)
(*     (R.opt R.floats       "prob"    prob)   ] *)

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


(* (\* class type ['a] compound = object *\) *)
(* (\*   method component : 'b. string -> 'b R.t *\) *)
(* (\* end *\) *)

(* (\* class ['b] listing r = object (self) *\) *)
(* (\*   inherit R.s3 r *\) *)
(* (\*   method names = R.strings_of_t (self#attribute "names") *\) *)
(* (\*   method component : 'a. string -> 'a R.t = component r *\) *)
(* (\*   method compound : 'b compound = (self :> 'b compound) *\) *)
(* (\* end *\) *)

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


(* let length l = R.int_of_t (R.eval Stub.length [ R.arg (fun x -> x) l ]) *)



(* class type ['a] listing = object *)
(*   method subset2_s : 'b. string -> 'b R.t *)
(*   method subset2   : 'b. int -> 'b R.t *)
(*   method length : int R.t *)
(*   method ty : 'a *)
(* end *)

(* let to_list (listing : 'a list #listing R.t) = *)
(*   List.map *)
(*     R.cast *)
(*     (R.sexps_of_t (R.cast (listing : 'c R.t :> R.sexp) : R.sexp list R.t)) *)


(* class type ['a] dataframe = object *)
(*   inherit ['a] listing *)
(*   method subset_ii : 'b. int -> int -> 'b R.t *)
(*   method dim : float list R.t *)
(* end *)
