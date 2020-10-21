module C = Configurator.V1
module S = String

let cfig c =
  let default: C.Pkg_config.package_conf = {
      libs = ["-lR"; "-lRmath"];
      cflags = [] }
  in
  let rlib = C.Process.run c "pkg-config" ["--libs-only-L"; "--libs-only-l"; "libR"] in
  let rmlib = C.Process.run c "pkg-config" ["--libs-only-L"; "--libs-only-l"; "libRmath"] in
  let rflg = C.Process.run c "pkg-config" ["--cflags"; "libR"] in
  let rmflg = C.Process.run c "pkg-config" ["--cflags"; "libRmath"] in
  let chk = (rlib.exit_code, rmlib.exit_code, rflg.exit_code, rmflg.exit_code) in
  let lib = (S.split_on_char ' ' (S.trim rlib.stdout)) @ (S.split_on_char ' ' (S.trim rmlib.stdout)) in
  let flg = (S.split_on_char ' ' (S.trim rflg.stdout)) @ (S.split_on_char ' ' (S.trim rmflg.stdout)) in  
  let discover: C.Pkg_config.package_conf = {
      libs = lib;
      cflags = flg }
  in
  let conf =
    match chk with
    | (0, 0, 0, 0) -> discover
    | _ -> default
  in
  C.Flags.write_sexp "c_flags.sexp" conf.cflags;
  C.Flags.write_sexp "c_library_flags.sexp" conf.libs;;

C.main ~name:"OCaml-R" cfig;;
