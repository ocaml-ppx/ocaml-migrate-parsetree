let () =
  match Sys.argv with
  | [|_; ocaml_version; fname|] ->
    let rewrite =
      if Filename.basename fname = Printf.sprintf "ast_%s.ml" ocaml_version then
        Pp_rewrite.rewrite_all
      else
        Pp_rewrite.rewrite_version
    in
    let ic = open_in_bin fname in
    Printf.printf "# 1 %S\n" fname;
    rewrite ocaml_version (Lexing.from_channel ic)
  | _ ->
    Printf.eprintf "%s: <ocaml-version> <file-name>\n"
      Sys.executable_name;
    exit 2
