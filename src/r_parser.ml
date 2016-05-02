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
open Conversion

type parse_status =
  | Parse_Null
  | Parse_OK
  | Parse_Incomplete
  | Parse_Error
  | Parse_EOF

exception Parsing_failure of parse_status * string

let parse_status_of_int = function
  | 0 -> Parse_Null
  | 1 -> Parse_OK
  | 2 -> Parse_Incomplete
  | 3 -> Parse_Error
  | 4 -> Parse_EOF
  | _ -> assert false

external raw_parse_string : string -> int -> int * sexp = "ocamlr_parse_string"
let parse_string ?max statement =
  let error_code, sexp = raw_parse_string statement
    begin match max with None -> -1 | Some n -> n end in
  match parse_status_of_int error_code with
  | Parse_OK -> langsxps_of_t (cast sexp : rawvecsxp)
  | _ as status -> raise (Parsing_failure (status, statement))

let parse statement = List.hd (parse_string ~max:1 statement)

