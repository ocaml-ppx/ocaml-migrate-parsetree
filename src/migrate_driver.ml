open Migrate_parsetree.Versions
module Ast_io = Migrate_parsetree.Ast_io

(** {1 State a rewriter can access} *)

type config = {
  tool_name: string;
  include_dirs : string list;
  load_path : string list;
  debug : bool;
  for_package : string option;
}

type cookie = Cookie : 'types ocaml_version * 'types get_expression -> cookie

type cookies = (string, cookie) Hashtbl.t

let get_cookie table name version =
  match
    match Hashtbl.find table name with
    | result -> Some result
    | exception Not_found ->
        match Ast_mapper.get_cookie name with
        | None -> None
        | Some expr -> Some (Cookie ((module OCaml_current), expr))
  with
  | None -> None
  | Some (Cookie (version', expr)) ->
    Some ((migrate version' version).copy_expression expr)

let set_cookie table name version expr =
  Hashtbl.replace table name (Cookie (version, expr))

let apply_cookies table =
  Hashtbl.iter (fun name (Cookie (version, expr)) ->
      Ast_mapper.set_cookie name
        ((migrate version (module OCaml_current)).copy_expression expr)
    ) table

let initial_state () : config * cookies =
  {
    tool_name = Ast_mapper.tool_name ();
    include_dirs = !Clflags.include_dirs;
    load_path = !Config.load_path;
    debug = !Clflags.debug;
    for_package = !Clflags.for_package
  }, Hashtbl.create 3

(** {1 Registering rewriters} *)

type 'types rewriter = config -> cookies -> 'types get_mapper

type rewriter_group =
    Rewriters : 'types ocaml_version * 'types rewriter list -> rewriter_group

let uniq_rewriter = Hashtbl.create 7
let registered_rewriters = ref []

let uniq_arg = Hashtbl.create 7
let registered_args = ref []

type ('types, 'version, 'rewriter) is_rewriter =
  | Is_rewriter : ('types, 'types ocaml_version, 'types rewriter) is_rewriter

let add_rewriter
    (type types) (type version) (type rewriter)
    (Is_rewriter : (types, version, rewriter) is_rewriter)
    (version : version) (rewriter : rewriter) =
  let rec add_rewriter = function
  | [] -> [Rewriters (version, [rewriter])]
  | (Rewriters (version', rewriters) as x) :: xs ->
      match compare_ocaml_version version version' with
      | Eq -> Rewriters (version', rewriter :: rewriters) :: xs
      | Lt -> Rewriters (version, [rewriter]) :: x :: xs
      | Gt -> x :: add_rewriter xs
  in
  add_rewriter

let register ~name ?(args=[]) version rewriter =
  (* Validate name *)
  if name = "" then
    invalid_arg "Migrate_driver.register: name is empty";
  if Hashtbl.mem uniq_rewriter name then
    invalid_arg ("Migrate_driver.register: rewriter " ^ name ^ " has already been registered")
  else Hashtbl.add uniq_rewriter name ();
  (* Validate arguments *)
  List.iter (fun (arg_name, _, _) ->
      match Hashtbl.find uniq_arg arg_name with
      | other_rewriter ->
          invalid_arg (Printf.sprintf
                         "Migrate_driver.register: argument %s is used by %s and %s" arg_name name other_rewriter)
      | exception Not_found ->
          Hashtbl.add uniq_arg arg_name name
    ) args;
  (* Register *)
  registered_args := List.rev_append args !registered_args;
  registered_rewriters :=
    add_rewriter Is_rewriter version rewriter !registered_rewriters

(** {1 Accessing or running registered rewriters} *)

type ('types, 'version, 'tree) is_signature =
    Signature : ('types, 'types ocaml_version, 'types get_signature) is_signature

type ('types, 'version, 'tree) is_structure =
    Structure : ('types, 'types ocaml_version, 'types get_structure) is_structure

let rec rewrite_signature
  : type types version tree.
    config -> cookies ->
    (types, version, tree) is_signature -> version -> tree ->
    rewriter_group list -> Parsetree.signature
  = fun (type types) (type version) (type tree)
    config cookies
    (Signature : (types, version, tree) is_signature)
    (version : version)
    (tree : tree)
    -> function
      | [] -> (migrate version (module OCaml_current)).copy_signature tree
      | Rewriters (version', rewriters) :: rest ->
          let rewrite rewriter tree =
            let (module Version) = version' in
            Version.Ast.map_signature (rewriter config cookies) tree
          in
          let tree = (migrate version version').copy_signature tree in
          let tree = List.fold_right rewrite rewriters tree in
          rewrite_signature config cookies Signature version' tree rest

let rec rewrite_structure
  : type types version tree.
    config -> cookies ->
    (types, version, tree) is_structure -> version -> tree ->
    rewriter_group list -> Parsetree.structure
  = fun (type types) (type version) (type tree)
    config cookies
    (Structure : (types, version, tree) is_structure)
    (version : version)
    (tree : tree)
    -> function
      | [] -> (migrate version (module OCaml_current)).copy_structure tree
      | Rewriters (version', rewriters) :: rest ->
          let rewriter rewriter tree =
            let (module Version) = version' in
            Version.Ast.map_structure (rewriter config cookies) tree
          in
          let tree = (migrate version version').copy_structure tree in
          let tree = List.fold_right rewriter rewriters tree in
          rewrite_structure config cookies Structure version' tree rest

let run_as_ast_mapper args =
  let spec = List.rev !registered_args in
  let args = Array.of_list ("" :: args) in
  let me = Filename.basename Sys.executable_name in
  let usage = Printf.sprintf "%s [options] [<files>]" me in
  match
    Arg.parse_argv args spec
      (fun arg -> raise (Arg.Bad (Printf.sprintf "invalid argument %S" arg)))
      usage
  with
  | exception (Arg.Help msg) ->
      prerr_endline msg;
      exit 1
  | () ->
      OCaml_current.Ast.make_top_mapper
        ~signature:(fun sg ->
            let config, cookies = initial_state () in
            let sg = rewrite_signature config cookies
                Signature (module OCaml_current) sg !registered_rewriters in
            apply_cookies cookies;
            sg
          )
        ~structure:(fun str ->
            let config, cookies = initial_state () in
            let str = rewrite_structure config cookies
                Structure (module OCaml_current) str !registered_rewriters in
            apply_cookies cookies;
            str
          )

let protectx x ~finally ~f =
  match f x with
  | y -> finally x; y
  | exception e -> finally x; raise e

let with_file_in fn ~f =
  protectx (open_in_bin fn) ~finally:close_in ~f

let with_file_out fn ~f =
  protectx (open_out_bin fn) ~finally:close_out ~f

type ('a, 'b) intf_or_impl =
  | Intf of 'a
  | Impl of 'b

let guess_file_kind fn =
  if Filename.check_suffix fn ".ml" then
    Impl fn
  else if Filename.check_suffix fn ".mli" then
    Intf fn
  else
    Location.raise_errorf ~loc:(Location.in_file fn)
      "I can't decide whether %s is an implementation or interface file"
      fn

let check_kind fn ~expected ~got =
  let describe = function
    | Intf _ -> "interface"
    | Impl _ -> "implementation"
  in
  match expected, got with
  | Impl _, Impl _
  | Intf _, Intf _ -> ()
  | _ ->
    Location.raise_errorf ~loc:(Location.in_file fn)
      "Expected an %s got an %s instead"
      (describe expected)
      (describe got)

let load_file file =
  let fn =
    match file with
    | Intf fn -> fn
    | Impl fn -> fn
  in
  with_file_in fn ~f:(fun ic ->
    match Ast_io.from_channel ic with
    | Ok (fn, Ast_io.Intf ((module V), sg)) ->
      check_kind fn ~expected:file ~got:(Intf ());
      (* We need to convert to the current version in order to interpret the cookies using
         [Ast_mapper.drop_ppx_context_*] from the compiler *)
      (fn, Intf ((migrate (module V) (module OCaml_current)).copy_signature sg))
    | Ok (fn, Ast_io.Impl ((module V), st)) ->
      check_kind fn ~expected:file ~got:(Impl ());
      (fn, Impl ((migrate (module V) (module OCaml_current)).copy_structure st))
    | Error (Ast_io.Unknown_version _) ->
      Location.raise_errorf ~loc:(Location.in_file fn)
        "File is a binary ast for an unknown version of OCaml"
    | Error (Ast_io.Not_a_binary_ast prefix_read_from_file) ->
      (* To test if a file is a binary AST file, we have to read the first few bytes of
         the file.

         If it is not a binary AST, we have to parse these bytes and the rest of the file
         as source code. To do that, we prefill the lexbuf buffer with what we read from
         the file to do the test. *)
      let lexbuf = Lexing.from_channel ic in
      let len = String.length prefix_read_from_file in
      String.blit prefix_read_from_file 0 lexbuf.Lexing.lex_buffer 0 len;
      lexbuf.Lexing.lex_buffer_len <- len;
      lexbuf.Lexing.lex_curr_p <-
        { Lexing.
          pos_fname = fn
        ; pos_lnum  = 1
        ; pos_bol   = 0
        ; pos_cnum  = 0
        };
      Location.input_name := fn;
      if Filename.check_suffix fn ".ml" then
        (fn, Impl (Parse.implementation lexbuf))
      else if Filename.check_suffix fn ".mli" then
        (fn, Intf (Parse.interface lexbuf))
      else
        (* TODO: add support for -intf and -impl *)
        Location.raise_errorf ~loc:(Location.in_file fn)
          "I can't decide whether %s is an implementation or interface file"
          fn)

let with_output output ~f =
  match output with
  | None -> f stdout
  | Some fn -> with_file_out fn ~f

let process_file ~config ~output ~dump_ast file =
  let cookies = Hashtbl.create 3 in
  let fn, ast = load_file file in
  let ast =
    match ast with
    | Intf sg ->
      let sg = Ast_mapper.drop_ppx_context_sig ~restore:true sg in
      let sg =
        rewrite_signature config cookies Signature
          (module OCaml_current) sg !registered_rewriters
      in
      apply_cookies cookies;
      Intf (sg, Ast_mapper.add_ppx_context_sig ~tool_name:config.tool_name sg)
    | Impl st ->
      let st = Ast_mapper.drop_ppx_context_str ~restore:true st in
      let st =
        rewrite_structure config cookies Structure
          (module OCaml_current) st !registered_rewriters
      in
      apply_cookies cookies;
      Impl (st, Ast_mapper.add_ppx_context_str ~tool_name:config.tool_name st)
  in
  with_output output ~f:(fun oc ->
    if dump_ast then begin
      let ast =
        match ast with
        | Intf (_, sg) -> Ast_io.Intf ((module OCaml_current), sg)
        | Impl (_, st) -> Ast_io.Impl ((module OCaml_current), st)
      in
      Ast_io.to_channel oc fn ast
    end else begin
      let ppf = Format.formatter_of_out_channel oc in
      (match ast with
       | Intf (sg, _) -> Pprintast.signature ppf sg
       | Impl (st, _) -> Pprintast.structure ppf st);
      Format.pp_print_newline ppf ()
    end)

let run_as_standalone_driver () =
  let output = ref None in
  let dump_ast = ref false in
  let files = ref [] in
  let set_cookie s =
    match String.index s '=' with
    | exception _ ->
      raise (Arg.Bad "invalid cookie, must be of the form \"<name>=<expr>\"")
    | i ->
      let name = String.sub s 0 i in
      let value = String.sub s (i + 1) (String.length s - i - 1) in
      let input_name = "<command-line>" in
      Location.input_name := input_name;
      let lexbuf = Lexing.from_string value in
      lexbuf.Lexing.lex_curr_p <-
        { Lexing.
          pos_fname = input_name
        ; pos_lnum  = 1
        ; pos_bol   = 0
        ; pos_cnum  = 0
        };
      let expr = Parse.expression lexbuf in
      Ast_mapper.set_cookie name expr
  in
  let spec =
    let as_ppx () =
      raise (Arg.Bad "--as-ppx must be passed as first argument")
    in
    [ "--as-ppx", Arg.Unit as_ppx,
      " Act as a -ppx rewriter"
    ; "--dump-ast", Arg.Set dump_ast,
      " Output a binary AST instead of source code"
    ; "-o", Arg.String (fun o -> output := Some o),
      "FILE Output to this file instead of the standard output"
    ; "--intf", Arg.String (fun fn -> files := Intf fn :: !files),
      "FILE Treat FILE as a .mli file"
    ; "--impl", Arg.String (fun fn -> files := Impl fn :: !files),
      "FILE Treat FILE as a .ml file"
    ; "--cookie", Arg.String set_cookie,
      "NAME=EXPR Set the cookie NAME to EXPR"
    ]
  in
  let spec = Arg.align (spec @ List.rev !registered_args) in
  let me = Filename.basename Sys.executable_name in
  let usage = Printf.sprintf "%s [options] [<files>]" me in
  try
    Arg.parse spec (fun anon -> files := guess_file_kind anon :: !files) usage;
    let output = !output in
    let dump_ast = !dump_ast in
    let config =
      (* TODO: we could add -I, -L and -g options to populate these fields. *)
      { tool_name    = "migrate_driver"
      ; include_dirs = []
      ; load_path    = []
      ; debug        = false
      ; for_package  = None
      }
    in
    List.iter (process_file ~config ~output ~dump_ast) (List.rev !files)
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 1

let run_as_ppx_rewriter () =
  Ast_mapper.run_main run_as_ast_mapper;
  exit 0

let run_main () =
  if Array.length Sys.argv >= 2 && Sys.argv.(1) = "--as-ppx" then
    run_as_ppx_rewriter ()
  else
    run_as_standalone_driver ();
  exit 0
