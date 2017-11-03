(**  Runtime R base library. *)

open OCamlR

val length : < length : R.integer R.t ; .. > R.t -> int
val dim : < dim : R.integers R.t ; .. > R.t -> int * int

val subset : < subset : 'b. R.integer R.t -> 'b R.t ; .. > R.t -> int -> 'b R.t
val subset_ii : < subset_ii : 'b. R.integer R.t -> R.integer R.t -> 'b R.t ; .. > R.t -> int -> int -> 'b R.t
val subset2_s : < subset2_s : 'b. R.string_ R.t -> 'b R.t ; .. > R.t -> string -> 'b R.t
val subset2_i : < subset2_i : 'b. R.integer R.t -> 'b R.t ; .. > R.t -> int -> 'b R.t

(* val rle : 'a R.scalar_format -> 'a list -> (int list * 'a list) *)

(**  Sampling function. *)
val sample :
  (< length : R.integer R.t ; subset : 'b. R.integer R.t -> 'b R.t ; .. > as 'c) R.t ->
  int ->
  ?replace:bool ->
  ?prob:R.reals R.t ->
  unit ->
  'c R.t

(* (\**  Lapply function, somewhat like List.map.*\) *)
(* val lapply : 'a list R.t -> 'b R.t -> 'c list R.t *)


(* class array_ : array_ R.t -> object *)
(*   inherit R.s3 *)
(*   method dim : float list R.t *)
(* end *)

(* class matrix : matrix R.t -> object *)
(*   inherit array_ *)
(*   method floats : float array array *)
(* end *)

(* val matrix : ?byrow:bool -> nrow:int -> ncol:int -> float list -> matrix R.t *)

(* val matrix_by_rows : float list list -> matrix R.t *)


(* class type ['a] listing = object *)
(*   method subset2_s : 'b. string -> 'b R.t *)
(*   method subset2   : 'b. int -> 'b R.t *)
(*   method length : int R.t *)
(*   method ty : 'a *)
(* end *)

(* val to_list : 'a list #listing R.t -> 'a R.t list *)

(* class type ['a] dataframe = object *)
(*   inherit ['a] listing *)
(*   method subset_ii : 'b. int -> int -> 'b R.t *)
(*   method dim : float list R.t *)
(* end *)


(* (\* type 'a compound = private < component : 'b. string -> 'b R.t ; .. > *\) *)
(* (\* val component : 'a compound R.t -> string -> 'b R.t *\) *)

(* (\* (\\**  Virtual class for R list S3 objects. *\\) *\) *)
(* (\* class ['a] listing : 'a listing R.t -> object *\) *)
(* (\*   inherit R.s3 *\) *)
(* (\*   method component : 'b. string -> 'b R.t *\) *)
(* (\*   method names : string list *\) *)
(* (\*   method ty : 'a compound *\) *)
(* (\* end *\) *)

(* (\* val listing : 'a listing R.t -> 'a listing *\) *)

(* (\* (\\**  Virtual class for R data frame S3 objects. *\\) *\) *)
(* (\* class ['a] dataframe : 'a dataframe R.t -> object *\) *)
(* (\*   inherit ['a] listing *\) *)
(* (\*   method row_names : string list *\) *)
(* (\*   method column : 'a. int -> 'a R.t *\) *)
(* (\*   method element : 'a. int -> int -> 'a R.t *\) *)
(* (\* end *\) *)

(* (\* val dataframe : 'a dataframe R.t -> 'a dataframe *\) *)

(* (\* (\\** Virtual class for dates in R. *\\) *\) *)
(* (\* class date : date R.t -> object *\) *)
(* (\*   inherit R.s3  *\) *)
(* (\*   method as_float : float *\) *)
(* (\*   method as_date : CalendarLib.Calendar.Date.t *\) *)
(* (\* end *\) *)

(* (\* type 'a compound *\) *)




















