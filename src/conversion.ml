(*********************************************************************************)
(*                OCaml-R                                                        *)
(*                                                                               *)
(*    Copyright (C) 2008-2010 Institut National de Recherche en                  *)
(*    Informatique et en Automatique. All rights reserved.                       *)
(*                                                                               *)
(*    Copyright (C) 2009-2010 Guillaume Yziquel. All rights reserved.            *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation; either version 3 of the         *)
(*    License, or  any later version.                                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public                  *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*             guillaume.yziquel@citycable.ch                                    *)
(*********************************************************************************)

open Sexptype
open Sexprec
open Read_internal
open Write_internal
open Allocation
open Data

let rec list_of_pairlist (ll : 'a internallist) =
  match sexptype (ll : 'a internallist :> sexp) with
  | NilSxp -> [] | ListSxp | LangSxp | DotSxp ->
  (* There's a typing issue with the DotSxp sexptype... TODO *)
  let ll : 'a listsxp = cast_to_sxp (ll : 'a internallist :> sexp) in
  ( (cast_to_sxp (inspect_listsxp_tagval ll) : symsxp (* TODO: This may be excessive *)), (inspect_listsxp_carval ll))
  :: (list_of_pairlist (cast_to_sxp (inspect_listsxp_cdrval ll) : pairlist))
  | _ -> failwith "Conversion failure in list_of_listsxp."

let pairlist_of_list (l: (sexp * sexp) list) =
  let r_l = alloc_list (List.length l) in
  let cursor = ref r_l in List.iter
  begin function (tag, value) ->
    let () = write_listsxp_element (cast_to_sxp (!cursor : pairlist :> sexp) : pairlistsxp) tag value in
    cursor := (cast_to_sxp (inspect_listsxp_cdrval (cast_to_sxp (!cursor : pairlist :> sexp) : pairlistsxp)) : pairlist)
  end l; r_l

external cons : sexp -> pairlist -> pairlistsxp = "ocamlr_cons"
external tag : pairlistsxp -> string -> unit = "ocamlr_tag"
external set_langsxp : pairlistsxp -> unit = "ocamlr_set_langsxp"

let langsxp (f: sexp) (args: (string option * sexp) list) : langsxp =
  let lcons hd tl = let x = cons hd tl in set_langsxp x; (cast_to_sxp (x : pairlistsxp :> sexp) : langsxp) in
  lcons f begin List.fold_right begin fun (t, hd) tl ->
    let x = cons hd tl in match t with
    | None -> (cast_to_sxp (x : pairlistsxp :> sexp) : pairlist)
    | Some name -> tag x name; (cast_to_sxp (x : pairlistsxp :> sexp) : pairlist)
  end args ((null_creator ()) : nilsxp :> pairlist) end

external string_of_charsxp : charvecsxp -> string = "ocamlr_internal_string_of_charsxp"

let list_of_vecsxp (access : 'a vecsxp -> int -> 'b) (s : 'a vecsxp) =
  let lngth = length_of_vecsxp s in
  let rec aux n s = match n with | 0 -> [] | _ ->
    let x = access s (lngth - n) in x::(aux (n - 1) s)
  in aux lngth s

let vecsxp_of_list (alloc : int -> 'a vecsxp) (assign : 'a vecsxp -> int -> 'b -> unit) (l: 'b list) =
  let s = alloc (List.length l) in
  let rec aux offset = function | [] -> () | hd::tl ->
    let () = assign s offset hd in aux (1 + offset) tl
  in aux 0 l; s

let bool_list_of_lglvecsxp x = list_of_vecsxp access_lglvecsxp x
let lglvecsxp_of_bool_list x = vecsxp_of_list alloc_lgl_vector assign_lglvecsxp x
let bools_of_t tau = bool_list_of_lglvecsxp (cast_to_sxp (tau : bool list t :> sexp) : lglvecsxp)
let bool_of_t tau = access_lglvecsxp (cast_to_sxp (tau : bool t :> sexp) : lglvecsxp) 0
  (* We access only the first element, because static typing is supposed to
     ensure that the lgl vecsxp contains only one element. *)
let bool b = (cast ((lglvecsxp_of_bool_list [b]) : lglvecsxp :> sexp) : bool t)
let bools bl = (cast ((lglvecsxp_of_bool_list bl) : lglvecsxp :> sexp) : bool list t)

let int_list_of_intvecsxp x  = list_of_vecsxp access_intvecsxp x
let intvecsxp_of_int_list x  = vecsxp_of_list alloc_int_vector assign_intvecsxp x
let ints_of_t tau = int_list_of_intvecsxp (cast_to_sxp (tau : int list t :> sexp) : intvecsxp)
let int_of_t tau = access_intvecsxp (cast_to_sxp (tau : int t :> sexp) : intvecsxp) 0
  (* We access only the first element, because static typing is supposed to
     ensure that the int vecsxp contains only one element. *)
let int i = (cast ((intvecsxp_of_int_list [i]) : intvecsxp :> sexp) : int t)
let ints il = (cast ((intvecsxp_of_int_list il) : intvecsxp :> sexp) : int list t)

let float_list_of_realvecsxp x = list_of_vecsxp access_realvecsxp x
let realvecsxp_of_float_list x = vecsxp_of_list alloc_real_vector assign_realvecsxp x
let floats_of_t tau = float_list_of_realvecsxp (cast_to_sxp (tau : float list t :> sexp) : realvecsxp)
let float_of_t tau = access_realvecsxp (cast_to_sxp (tau : float t :> sexp) : realvecsxp) 0
  (* We access only the first element, because static typing is supposed to
     ensure that the real vecsxp contains only one element. *)
let float x = (cast ((realvecsxp_of_float_list [x]) : realvecsxp :> sexp) : float t)
let floats xl = (cast ((realvecsxp_of_float_list xl) : realvecsxp :> sexp) : float list t)

let realvecsxp_of_float_option_list x = vecsxp_of_list alloc_real_vector assign_realvecsxp_opt x
let optfloats xl = (cast ((realvecsxp_of_float_option_list xl) : realvecsxp :> sexp) : float option list t)

let string_list_of_strvecsxp x = list_of_vecsxp access_strvecsxp x 
let strvecsxp_of_string_list x = vecsxp_of_list alloc_str_vector assign_strvecsxp x
let strings_of_t tau = string_list_of_strvecsxp (cast_to_sxp (tau : string list t :> sexp) : strvecsxp)
let string_of_t tau = access_strvecsxp (cast_to_sxp (tau : string t :> sexp) : strvecsxp) 0
  (* We access only the first element, because static typing is supposed to
     ensure that the str vecsxp contains only one element. *)
external string : string -> string t = "ocamlr_strsxp_of_string"
let strings sl = (cast ((strvecsxp_of_string_list sl) : strvecsxp :> sexp) : string list t)

let sexp_list_of_rawvecsxp x = list_of_vecsxp access_rawvecsxp x
let sexps_of_t tau = sexp_list_of_rawvecsxp (cast_to_sxp (tau : sexp list t :> sexp) : rawvecsxp)

let langsxp_list_of_exprvecsxp x = list_of_vecsxp access_exprvecsxp x
let langsxps_of_t tau = langsxp_list_of_exprvecsxp (cast_to_sxp (tau : langsxp list t :> sexp) : exprvecsxp)



















