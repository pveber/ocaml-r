(* OASIS_START *)
(* OASIS_STOP *)
open Ocamlbuild_plugin

let is_space = function
| ' ' | '\012' | '\n' | '\r' | '\t' -> true
| _ -> false

let trim s = String.(
  let len = length s in
  let i = ref 0 in
  while !i < len && is_space (unsafe_get s !i) do
    incr i
  done;
  let j = ref (len - 1) in
  while !j >= !i && is_space (unsafe_get s !j) do
    decr j
  done;
  if !i = 0 && !j = len - 1 then
    s
  else if !j >= !i then
    sub s !i (!j - !i + 1)
  else ""
)

let read_lines_from_cmd ~max_lines cmd =
  let ic = Unix.open_process_in cmd in
  let lines_ref = ref [] in
  let rec loop n =
    if n <= 0 then ()
    else begin
      lines_ref := input_line ic :: !lines_ref;
      loop (n - 1)
    end
  in
  begin
    try loop max_lines with
    | End_of_file -> ()
    | exc -> close_in_noerr ic; raise exc
  end;
  close_in ic;
  List.rev !lines_ref

let () =
  dispatch
    (fun hook ->
      dispatch_default hook;
      match hook with
      | After_rules ->
          rule "standard module generation"
            ~prods:["src/standard.ml"]
            ~deps:["src/standard.R"]
            begin fun _ _ ->
              Cmd(S[A"R"; A"--silent"; A"--vanilla"; A"--slave"; Sh"<"; P"src/standard.R"; Sh">"; Px"src/standard.ml"])
            end ;

          (* A hack taken from sqlite3 binding. To be removed when
             pkg-config is there *)
          let rec split_string s =
            match try Some (String.index s ' ') with Not_found -> None with
            | Some pos ->
                let before = trim (String.before s pos) in
                let after = String.after s (pos + 1) in
                if before = "" then split_string after
                else before :: split_string after
            | None when s = "" -> []
            | None -> [s]
          in
          let ocamlify ~ocaml_flag flags =
            let chunks = split_string flags in
            let cnv flag = [A ocaml_flag; A flag] in
            List.concat (List.map cnv chunks)
          in
          let ldify flags =
            let chunks = split_string flags in
            let cnv flag =
              if String.length flag >= 2 && (let prefix = String.sub flag 0 2 in prefix = "-l" || prefix = "-L")
              then [ A flag ]
              else [ A "-ldopt" ; A flag ]
            in
            List.concat (List.map cnv chunks)
          in
          let olibR_cflags =
            let cmd = "pkg-config --cflags libR" in
            match read_lines_from_cmd ~max_lines:1 cmd with
            | [cflags] -> S (ocamlify ~ocaml_flag:"-ccopt" cflags)
            | _ -> failwith "pkg-config failed for cflags"
          in
          let libR_clibs, olibR_clibs =
            let cmd = "pkg-config --libs libR" in
            match read_lines_from_cmd ~max_lines:1 cmd with
            | [libs] ->
                S (ldify libs), S (ocamlify ~ocaml_flag:"-cclib" libs)
            | _ -> failwith "pkg-config failed for libs"
          in
          let olibRmath_cflags =
            let cmd = "pkg-config --cflags libRmath" in
            match read_lines_from_cmd ~max_lines:1 cmd with
            | [cflags] -> S (ocamlify ~ocaml_flag:"-ccopt" cflags)
            | _ -> failwith "pkg-config failed for cflags"
          in
          let libRmath_clibs, olibRmath_clibs =
            let cmd = "pkg-config --libs libRmath" in
            match read_lines_from_cmd ~max_lines:1 cmd with
            | [libs] ->
                S (ldify libs), S (ocamlify ~ocaml_flag:"-cclib" libs)
            | _ -> failwith "pkg-config failed for libs"
          in
          flag ["compile"; "c"] olibR_cflags;
          flag ["compile"; "c"] olibRmath_cflags;
          flag ["link"; "ocaml"; "library"] olibR_clibs;
          flag ["link"; "ocaml"; "library"] olibRmath_clibs;
          flag ["ocamlmklib"; "c"] libR_clibs;
          flag ["ocamlmklib"; "c"] libRmath_clibs;
          flag ["link"] olibR_clibs ;
          flag ["link"] olibRmath_clibs ;

          (* A hack for using the syntax extension internally *)
          flag ["ocaml"; "compile"; "pa_r"] & S[A"-ppopt"; A "src/syntax/R_syntax.cma" ];
          flag ["ocaml"; "link"; "pa_rscript"] & S[A "src/R_interpreter.cmo"; ];
          flag ["ocaml"; "ocamldep"; "pa_r"] & S[A"-ppopt"; A "src/syntax/R_syntax.cma"];
          flag ["ocaml"; "doc"; "pa_rscript"] & S[A"-ppopt"; A "src/syntax/R_syntax.cma"];
          dep ["ocaml"; "ocamldep"; "pa_r"] ["src/syntax/R_syntax.cma"]


      | _ ->
          ())
