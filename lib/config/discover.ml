module C = Configurator.V1
module S = String

let trim_to_list str =
  S.split_on_char ' ' (S.trim str)

let ( ++ ) maybe_x maybe_y =
  match maybe_x, maybe_y with
  | Some x, Some y -> Some (x @ y)
  | _ -> maybe_x

let default : C.Pkg_config.package_conf = {
  libs = ["-lR"; "-lRmath"];
  cflags = []
}

let cfig c =
  let pkg_config args =
    let res = C.Process.run c "pkg-config" args in
    if res.exit_code = 0 then Some (trim_to_list res.stdout) else None
  in
  let pkg_config_lib x = pkg_config ["--libs-only-L"; "--libs-only-l"; x] in
  let pkg_config_flg x = pkg_config ["--cflags"; x] in
  let lib = pkg_config_lib "libR" ++ pkg_config_lib "libRmath" in
  let cflags = pkg_config_flg "libR" ++ pkg_config_flg "libRmath" in
  let conf : C.Pkg_config.package_conf =
    match lib, cflags with
    | Some libs, Some cflags -> { libs ; cflags }
    | _ -> default
  in
  C.Flags.write_sexp "c_flags.sexp" conf.cflags;
  C.Flags.write_sexp "c_library_flags.sexp" conf.libs;;

C.main ~name:"OCaml-R" cfig;;
