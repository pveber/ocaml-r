module C = Configurator.V1

let cfig  c =
  let default = { C.Pkg_config.libs = ["-lR"]; cflags = [] } in
  let conf =
    match C.Pkg_config.get c with
    | None -> default
    | Some pc ->
       match C.Pkg_config.query pc ~package:"libR libRmath" with
       | None -> default
       | Some deps -> deps in

  C.Flags.write_sexp "c_flags.sexp" conf.cflags;
  C.Flags.write_sexp "c_library_flags.sexp" conf.libs;;

C.main ~name:"OCaml-R" cfig;;
