{
open Printf

let print_ocaml_version version =
  let patt_len = String.length "OCAML_VERSION" in
  (* Note: the spaces in the replacements are to preserve locations *)
  printf "%-*s" patt_len version
}

rule rewrite_version ocaml_version = parse
  | "OCAML_VERSION"
    { print_ocaml_version ocaml_version;
      rewrite_version ocaml_version lexbuf
    }
  | _ as c
    { print_char c;
      rewrite_version ocaml_version lexbuf
    }
  | eof { () }

and rewrite_all ocaml_version = parse
  | "OCAML_VERSION"
    { print_ocaml_version ocaml_version;
      rewrite_all ocaml_version lexbuf
    }
  |                 "(*IF_CURRENT " ([^'*']* as s) "*)"
    { print_string ("             " ^ s ^           "  ");
      rewrite_all ocaml_version lexbuf
    }
  | _ as c
    { print_char c;
      rewrite_all ocaml_version lexbuf
    }
  | eof { () }


