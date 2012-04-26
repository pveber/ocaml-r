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

external force_promsxp : promsxp -> sexp = "ocamlr_eval_sxp"

(*let force : 'a promise -> 'a t = force_promsxp*)

(* For lazy evaluation, we have an issue here: R promises
   are recursively forced by eval. This means that the
   OCaml type system would be broken, because we would need
   to have 'a Lazy.t R.t = 'a Lazy.t Lazy.t R.t. There's two
   solutions:

   -1- 'a R.t would denote a R value of type 'a, lazy or not.
       This is suboptimal, because the OCaml type system could
       and should express these lazy semantics.

   -2- Make a dynamic check on the nature of the argument of
       the force function. If it is a lazy lazy value, we
       should force it manually, with OCaml semantics. If not,
       we can run eval on it. *)


