(*$ #use "src/cinaps_helpers" $*)
let usage_msg =
  Printf.sprintf "Usage: %s <input-ast> [-to-ocaml40x <output-ast>]"
    Sys.argv.(0)

type to_version =
  To : 'a Migrate_parsetree.Versions.ocaml_version -> to_version

let conversions = ref []
let input = ref ""

let () =
  let add v name = conversions := (v, name) :: !conversions in
  let set_input name =
    if !input = "" then input := name
    else
      raise (Arg.Bad (Printf.sprintf
                        "You can pass only one input filename (got %S and %S)"
                        !input name))
  in
  let arg_spec = [
    (*$ foreach_version (fun suffix version ->
          printf "(\"-to-ocaml%s\", Arg.String (add (To Migrate_parsetree.OCaml_%s)),\n" suffix suffix;
          printf "\"<filename> Produce an ast valid for OCaml %s in <filename>\");\n" version;
        )
    *)
    ("-to-ocaml402", Arg.String (add (To Migrate_parsetree.Versions.ocaml_402)),
     "<filename> Produce an ast valid for OCaml 4.02 in <filename>");
    ("-to-ocaml403", Arg.String (add (To Migrate_parsetree.Versions.ocaml_403)),
     "<filename> Produce an ast valid for OCaml 4.03 in <filename>");
    ("-to-ocaml404", Arg.String (add (To Migrate_parsetree.Versions.ocaml_404)),
     "<filename> Produce an ast valid for OCaml 4.04 in <filename>");
    ("-to-ocaml405", Arg.String (add (To Migrate_parsetree.Versions.ocaml_405)),
     "<filename> Produce an ast valid for OCaml 4.05 in <filename>");
    ("-to-ocaml406", Arg.String (add (To Migrate_parsetree.Versions.ocaml_406)),
     "<filename> Produce an ast valid for OCaml 4.06 in <filename>");
    ("-to-ocaml407", Arg.String (add (To Migrate_parsetree.Versions.ocaml_407)),
     "<filename> Produce an ast valid for OCaml 4.07 in <filename>");
    (*$*)
  ] in
  Arg.parse arg_spec set_input usage_msg;
  if !input = "" then (
    Arg.usage arg_spec usage_msg;
    exit 1
  );
  let (src_filename, ast) =
    let ic = open_in_bin !input in
    let result =
      try
        Migrate_parsetree.Ast_io.from_channel ic
      with exn ->
        close_in ic;
        raise exn
    in
    close_in ic;
    match result with
    | Error (Unknown_version number) ->
      Printf.eprintf "Input file has unknown magic number: %s\n" number;
      exit 1
    | Error (Not_a_binary_ast _) ->
      Printf.eprintf "Input file is not a binary AST\n";
      exit 1
    | Ok ast -> ast
  in
  let version =
    match ast with
    | Impl ((module O), _) -> O.string_version
    | Intf ((module O), _) -> O.string_version
  in
  Printf.printf "Ast of %S for OCaml %s\n" src_filename version;
  List.iter (fun (To (module Dst), dst_filename) ->
    let ast' : Migrate_parsetree.Ast_io.ast =
      match ast with
      | Impl ((module Src), src_impl) ->
          let module Convert = Migrate_parsetree.Versions.Convert(Src)(Dst) in
          let dst_impl = Convert.copy_structure src_impl in
          (Impl ((module Dst), dst_impl))
      | Intf ((module Src), src_sig) ->
          let module Convert = Migrate_parsetree.Versions.Convert(Src)(Dst) in
          let dst_sig = Convert.copy_signature src_sig in
          (Intf ((module Dst), dst_sig))
    in
    match
      let oc = open_out_bin dst_filename in
      Migrate_parsetree.Ast_io.to_channel oc src_filename ast';
      close_out_noerr oc
    with
    | () ->
      Printf.printf "Successfully converted %S to OCaml %s in %S\n"
        !input Dst.string_version dst_filename
    | exception exn ->
      Printf.eprintf "Failed to convert %S to OCaml %s in %S:\n%s%!\n"
        !input Dst.string_version dst_filename
        (Printexc.to_string exn)
  ) (List.rev !conversions)
