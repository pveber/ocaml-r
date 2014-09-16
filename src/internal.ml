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

open Data
open Read_internal
open Sexptype
open Sexprec
open Conversion

let is_nil x = sexptype (x : _ t :> sexp) = NilSxp
let nil_map x ~f =
  if is_nil x then None
  else Some (f x)

module Specification = struct

  type symbol = (string * (sexp option)) option option

  let of_symbol (s : symsxp) =
    let pname    = inspect_symsxp_pname    s
    and value    = inspect_symsxp_value    s
    and internal = inspect_symsxp_internal s in
    match (sexptype pname), (sexptype value), (sexptype internal) with
    | (NilSxp,  _, NilSxp) when sexp_equality (s : symsxp :> sexp) value -> None
    | (CharSxp, SymSxp, NilSxp) ->
        begin match (sexp_equality (s : symsxp :> sexp) value) &&
                    ("" = string_of_charsxp (cast_to_sxp pname : charvecsxp)) with
        | true -> Some None | false ->
        begin match (sexp_equality value (inspect_symsxp_value (cast_to_sxp value : symsxp)))  &&
                    (NilSxp = sexptype (inspect_symsxp_pname (cast_to_sxp value : symsxp)))    &&
                    (NilSxp = sexptype (inspect_symsxp_internal (cast_to_sxp value : symsxp))) with
        | true -> Some (Some ((string_of_charsxp (cast_to_sxp pname : charvecsxp)), None))
        | false -> assert false
        end end
    | (CharSxp, _, NilSxp) ->
        let symbol_name = string_of_charsxp (cast_to_sxp pname : charvecsxp) in
        Some (Some (symbol_name, (Some value)))
    | _ -> assert false

end

module type Types = sig

  type t
  val recursive : t Lazy.t -> t
  val build : (sexp -> t) -> sexp -> t

end

module Parsing (M : Types) = struct

  (* General parsing function for internal R structures, i.e. SEXPs. *)

  let t_of_sexp (s : sexp) =
    let rec aux sexps_seen s =
      let is_found (ss, _) = sexp_equality s ss in
      begin match (try Some (List.find is_found sexps_seen) with _ -> None) with
      | None -> let rec x = lazy (M.build (aux ((s, x)::sexps_seen)) s) in Lazy.force x
      | Some (_, t_lazy) -> M.recursive t_lazy
      end
    in aux [] s

end

module CTypes = struct

  (* Type definitions. *)

  type t = | Recursive of t Lazy.t | Val of t_val

  and t_val = {
    (* sxpinfo : sxpinfo;   *)
    (* attrib  : t;         *)
    (* gengc_nextnode : t;  *)
    (* gengc_prevnode : t;  *)
    content : t_content
  }

  (* and sxpinfo = {
    type  : sexptype;
    obj   : int;
    named : int;
    gp    : int;
    mark  : int;
    debug : int;
    trace : int;
    spare : int;
    gcgen : int;
    gccls : int;
  }*)

  and t_content =
    | NILSXP
    | SYMSXP of sxp_sym
    | LISTSXP of sxp_list
    | CLOSXP of sxp_clos
    | ENVSXP of sxp_env
    | PROMSXP of sxp_prom
    | LANGSXP of sxp_list
    | SPECIALSXP
    | BUILTINSXP of int
    | CHARSXP of string
    | LGLSXP of bool list
    | INTSXP of int list
    | REALSXP of float list
    | CPLXSXP
    | STRSXP of string list
    | DOTSXP
    | ANYSXP
    | VECSXP of t list
    | EXPRSXP
    | BCODESXP
    | EXTPTRSXP
    | WEAKREFSXP
    | RAWSXP
    | S4SXP
    | FUNSXP

  and sxp_sym  = { pname: t; sym_value: t; internal: t }
  and sxp_list = { carval: t; cdrval: t; tagval: t }
  and sxp_env  = { frame: t; (*enclos: t; hashtab: t*) }
  and sxp_clos = { formals: t; body: t; clos_env: t }
  and sxp_prom = { prom_value: t; expr: t; prom_env: t }

  let recursive x = Recursive (lazy (Lazy.force x))

  let build rec_build s =
    match sexptype s with
    | NilSxp     -> Val { content = NILSXP }
    | SymSxp     -> Val { content = SYMSXP {
        pname      = rec_build (inspect_symsxp_pname    (cast_to_sxp s : symsxp));
        sym_value  = rec_build (inspect_symsxp_value    (cast_to_sxp s : symsxp));
        internal   = rec_build (inspect_symsxp_internal (cast_to_sxp s : symsxp))}}
    | ListSxp    -> Val { content = LISTSXP {
        carval     = rec_build (inspect_listsxp_carval  (cast_to_sxp s : pairlistsxp));
        cdrval     = rec_build (inspect_listsxp_cdrval  (cast_to_sxp s : pairlistsxp));
        tagval     = rec_build (inspect_listsxp_tagval  (cast_to_sxp s : pairlistsxp))}}
    | CloSxp     -> Val { content = CLOSXP {
        formals    = rec_build (inspect_closxp_formals  (cast_to_sxp s : closxp));
        body       = rec_build (inspect_closxp_body     (cast_to_sxp s : closxp));
        clos_env   = rec_build (inspect_closxp_env      (cast_to_sxp s : closxp))}}
    | EnvSxp     -> Val { content = ENVSXP {
        frame      = rec_build (inspect_envsxp_frame    (cast_to_sxp s : envsxp));
     (* enclos     = rec_build (inspect_envsxp_enclos   s); *)
     (* hashtab    = rec_build (inspect_envsxp_hashtab  s) *) }}
    | PromSxp    -> Val { content = PROMSXP {
        prom_value = rec_build (inspect_promsxp_value  (cast_to_sxp s : promsxp));
        expr       = rec_build (inspect_promsxp_expr   (cast_to_sxp s : promsxp));
        prom_env   = rec_build (inspect_promsxp_env    (cast_to_sxp s : promsxp))}}
    | LangSxp    -> Val { content = LANGSXP {
        carval     = rec_build (inspect_listsxp_carval (cast_to_sxp s : langsxp));
        cdrval     = rec_build (inspect_listsxp_cdrval (cast_to_sxp s : langsxp));
        tagval     = rec_build (inspect_listsxp_tagval (cast_to_sxp s : langsxp))}}
    | SpecialSxp -> Val { content = SPECIALSXP }
    | BuiltinSxp -> Val { content = BUILTINSXP (inspect_primsxp_offset (cast_to_sxp s : builtinsxp))}
    | CharSxp    -> Val { content = CHARSXP (string_of_charsxp (cast_to_sxp s : charvecsxp)) }
    | LglSxp     -> Val { content = LGLSXP (bool_list_of_lglvecsxp (cast_to_sxp s : lglvecsxp))}
    | IntSxp     -> Val { content = INTSXP (int_list_of_intvecsxp (cast_to_sxp s : intvecsxp))}
    | RealSxp    -> Val { content = REALSXP (float_list_of_realvecsxp (cast_to_sxp s : realvecsxp))}
    | CplxSxp    -> Val { content = CPLXSXP }
    | StrSxp     -> Val { content = STRSXP (string_list_of_strvecsxp (cast_to_sxp s: strvecsxp))}
    | DotSxp     -> Val { content = DOTSXP }
    | AnySxp     -> Val { content = ANYSXP }
    | VecSxp     -> Val { content = VECSXP (List.map rec_build (sexp_list_of_rawvecsxp (cast_to_sxp s : rawvecsxp)))}
    | ExprSxp    -> Val { content = EXPRSXP }
    | BcodeSxp   -> Val { content = BCODESXP }
    | ExtptrSxp  -> Val { content = EXTPTRSXP }
    | WeakrefSxp -> Val { content = WEAKREFSXP }
    | RawSxp     -> Val { content = RAWSXP }
    | S4Sxp      -> Val { content = S4SXP }
    | FunSxp     -> Val { content = FUNSXP }

end

module PrettyTypes = struct

  type t =
    | Recursive of t Lazy.t
    | NULL
    | SYMBOL of (string * t) option
    | ARG of string
    | PLACE
    | LIST of pairlist
    | CLOSURE of closure
    | ENV of environment
    | PROMISE of promise
    | CALL of t * pairlist
    | SPECIAL of int
    | BUILTIN
    | STRING of string
    | STRINGS of string list
    | INTS of int list
    | VECSXP of t list
    | BOOLS of bool list
    | FLOATS of float list
    | Unknown

  and closure     = { formals: t; body: t; clos_env: t }
  and environment = { frame: t; (* enclos: t; hashtab: t *) }
  and promise     = { value: t; expr: t; prom_env: t }

  and pairlist = (t * t) list (* For strict list parsing, t list. *)

  let recursive x = Recursive (lazy (Lazy.force x))

  exception Sexp_to_inspect of sexp
  exception Esoteric of sexp

  let symbol_of_symsxp builder (s : symsxp) =
    match begin try Some (Specification.of_symbol s) with
                | Assert_failure _ -> None end with
    | None -> raise (Esoteric (s : symsxp :> sexp))
    | Some None -> SYMBOL None
    | Some (Some None) -> PLACE
    | Some (Some (Some (symbol_name, None))) -> ARG symbol_name
    | Some (Some (Some (symbol_name, Some v))) -> SYMBOL (Some (symbol_name, (builder v)))  

  let rec list_of_listsxp builder (s : 'a listsxp) =
    let carval = inspect_listsxp_carval s
    and cdrval = inspect_listsxp_cdrval s
    and tagval = inspect_listsxp_tagval s in
    (* Strict parsing of the LIST:
    LIST begin match sexptype tagval with
    | NilSxp ->  (builder carval) :: begin
                 match builder cdrval with
                 | LIST l -> l | NULL -> []
                 | _ -> raise (Esoteric s) end
    | _ -> raise Esoteric end *)
    (* Lax parsing of the LIST: *)
    LIST begin ((builder tagval), (builder carval))::
      begin match builder cdrval with
      | LIST l -> l | NULL -> []
      | _ -> raise (Esoteric (s : 'a listsxp :> sexp)) end
    end

  let rec build rec_build =
    let phi = fun f -> f (build rec_build) in
    function s -> match sexptype s with
    | NilSxp     -> NULL
    | SymSxp     -> begin try phi symbol_of_symsxp (Obj.magic s) with
                    | Esoteric _ -> Unknown end
    | ListSxp    -> begin try phi list_of_listsxp (cast_to_sxp s : 'a listsxp) with
                    | Esoteric _ -> Unknown end
    | CloSxp     -> CLOSURE {
        formals  = rec_build (inspect_closxp_formals (cast_to_sxp s : closxp));
        body     = rec_build (inspect_closxp_body    (cast_to_sxp s : closxp));
        clos_env = rec_build (inspect_closxp_env     (cast_to_sxp s : closxp))}
    | EnvSxp     -> ENV {
        frame    = rec_build (inspect_envsxp_frame   (cast_to_sxp s : envsxp));
     (* enclos  = rec_build (inspect_envsxp_enclos  s); *) (* We do not care for now. *)
     (* hashtab = rec_build (inspect_envsxp_hashtab s)  *) }
    | PromSxp    -> PROMISE {
        value    = rec_build (inspect_promsxp_value  (cast_to_sxp s : promsxp));
        expr     = rec_build (inspect_promsxp_expr   (cast_to_sxp s : promsxp));
        prom_env = rec_build (inspect_promsxp_env    (cast_to_sxp s : promsxp))}
    | LangSxp    ->
        let carval = inspect_listsxp_carval (cast_to_sxp s : langsxp)
        and cdrval = inspect_listsxp_cdrval (cast_to_sxp s : langsxp)
        and tagval = inspect_listsxp_tagval (cast_to_sxp s : langsxp) in
        begin match build rec_build cdrval with
        | LIST l -> begin match sexptype tagval with
                    | NilSxp -> CALL ((build rec_build carval), l)
                    | _ -> Unknown end
        | _ -> Unknown end
    | SpecialSxp -> SPECIAL (inspect_primsxp_offset (cast_to_sxp s : specialsxp))
    | BuiltinSxp -> BUILTIN
    | CharSxp    -> STRING  (string_of_charsxp (cast_to_sxp s : charvecsxp))
    | LglSxp     -> BOOLS   (bool_list_of_lglvecsxp (cast_to_sxp s : lglvecsxp))
    | IntSxp     -> INTS    (int_list_of_intvecsxp (cast_to_sxp s : intvecsxp))
    | RealSxp    -> FLOATS  (float_list_of_realvecsxp (cast_to_sxp s : realvecsxp))
    | CplxSxp    -> Unknown
    | StrSxp     -> STRINGS (string_list_of_strvecsxp (cast_to_sxp s : strvecsxp))
    | DotSxp     -> Unknown
    | AnySxp     -> Unknown
    | VecSxp     -> VECSXP  (List.map rec_build (sexp_list_of_rawvecsxp (cast_to_sxp s : rawvecsxp)))
    | ExprSxp    -> Unknown
    | BcodeSxp   -> Unknown
    | ExtptrSxp  -> Unknown
    | WeakrefSxp -> Unknown
    | RawSxp     -> Unknown
    | S4Sxp      -> Unknown
    | FunSxp     -> Unknown

end

module CParsed = Parsing (CTypes)

module PrettyParsed = Parsing (PrettyTypes)

module C = struct
  include CTypes
  include CParsed
end

module Pretty = struct
  include PrettyTypes
  include PrettyParsed
end

