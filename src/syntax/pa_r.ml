(* 
 * Adapted from: 
 *
 * Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

let rnd = Random.State.make [|0x513511d4|]
let random_var () =
  Format.sprintf "a%08Lx" (Random.State.int64 rnd 0x100000000L)

open Camlp4

module Id : Sig.Id = struct
  let name = "R"
  let version = "1.0"
end

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig
  include Syntax

  let rec filter stream =
    match stream with parser
      [< '(KEYWORD "#", loc); rest >] ->
        begin match rest with parser
          [< '(KEYWORD "#", loc') >] ->
             [< '(KEYWORD "##", Loc.merge loc loc'); filter rest >]
        | [< >] ->
             [< '(KEYWORD "#", loc); filter rest >]
        end
    | [< 'other; rest >] -> [< 'other; filter rest >]

  let _ =
    Token.Filter.define_filter (Gram.get_filter ())
      (fun old_filter stream -> old_filter (filter stream))


  let fresh_type _loc = <:ctyp< '$random_var ()$ >>

  let string_map f s = String.(
    let r = copy s in
    for i = 0 to length s - 1 do
      r.[i] <- f s.[i]
    done ;
    r
  )

  let id_ml2r id = 
    let r = string_map (function '\'' -> '.' | x -> x) id in 
    String.(
      if length r > 0 && r.[0] = '_' then 
        sub r 1 (length r - 1)
      else r
    )

  let access_object e m m_loc comp_type f =
    let _loc = Ast.loc_of_expr e in
    let x = random_var () in
    let obj_type = fresh_type _loc in
    let constr =
      let y = random_var () in
      <:expr< fun () -> let (_ : Rbase.compound (< .. > as $obj_type$)) = $lid:x$#compound in 
                        fun ($lid:y$ : $obj_type$) -> ($lid:y$#$m$ : $comp_type$) >>
    in
    <:expr< let $lid:x$ = $e$ in
            let _ = $constr$ in
            $f x$ >>


  let jsmeth = Gram.Entry.mk "jsmeth"

  EXTEND Gram
    jsmeth: [["##"; lab = label -> (_loc, lab) ]];
    expr: BEFORE "."
    ["##" RIGHTA
     [ e = SELF; (lab_loc, lab) = jsmeth ->
         let comp_type = fresh_type _loc in
         access_object e lab lab_loc comp_type (fun x ->
         <:expr< ($lid:x$#component $str:id_ml2r lab$ : $comp_type$) >>)
     ]];
  END

(*XXX n-ary methods

how to express optional fields?  if they are there, they must have
some type, but  they do not have to be there

use variant types instead of object types?
   in a negative position...  (but then we have to negate again...)

    { foo: "bar", baz : 7 } : [`foo of string field | `baz of int field] obj

    let f (x : t) = (x : [< `foo of string field | `baz of int field| `x of string field] obj)


XXXX
module WEIRDMODULENAME = struct type 'a o = 'a Js.t val unsafe_get = Js.Unsafe.get ... end
(let module M = WEIRDMODULENAME in (M.unsafe_get : <x : 'a M.meth> -> 'a))

XXXX be more careful with error messages:
  put coercions against arguments or whole expression
*)

end

module M = Register.OCamlSyntaxExtension(Id)(Make)
