open Migrate_parsetree.Versions

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

type 'types rewriter = config -> cookies -> string list -> 'types get_mapper

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
    config -> cookies -> string list ->
    (types, version, tree) is_signature -> version -> tree -> rewriter_group list -> Parsetree.signature
  = fun (type types) (type version) (type tree)
    config cookies args
    (Signature : (types, version, tree) is_signature)
    (version : version)
    (tree : tree)
    -> function
      | [] -> (migrate version (module OCaml_current)).copy_signature tree
      | Rewriters (version', rewriters) :: rest ->
          let tree = (migrate version version').copy_signature tree in
          let tree =
            List.fold_right (fun rewriter tree ->
                let (module Version) = version' in
                Version.Ast.map_signature (rewriter config cookies args) tree)
              rewriters tree
          in
          rewrite_signature config cookies args Signature version' tree rest

let rec rewrite_structure
  : type types version tree.
    config -> cookies -> string list ->
    (types, version, tree) is_structure -> version -> tree -> rewriter_group list -> Parsetree.structure
  = fun (type types) (type version) (type tree)
    config cookies args
    (Structure : (types, version, tree) is_structure)
    (version : version)
    (tree : tree)
    -> function
      | [] -> (migrate version (module OCaml_current)).copy_structure tree
      | Rewriters (version', rewriters) :: rest ->
          let tree = (migrate version version').copy_structure tree in
          let tree =
            List.fold_right (fun rewriter tree ->
                let (module Version) = version' in
                Version.Ast.map_structure (rewriter config cookies args) tree)
              rewriters tree
          in
          rewrite_structure config cookies args Structure version' tree rest

let run_as_ast_mapper args =
  let spec = List.rev !registered_args in
  let args = Array.of_list ("" :: args) in
  let anons = ref [] in
  Arg.parse_argv args spec (fun anon -> anons := anon :: !anons) "usage";
  let anons = List.rev !anons in
  OCaml_current.Ast.make_top_mapper
    ~signature:(fun sg ->
        let config, cookies = initial_state () in
        let sg = rewrite_signature config cookies anons Signature (module OCaml_current) sg !registered_rewriters in
        apply_cookies cookies;
        sg
      )
    ~structure:(fun str ->
        let config, cookies = initial_state () in
        let str = rewrite_structure config cookies anons Structure (module OCaml_current) str !registered_rewriters in
        apply_cookies cookies;
        str
      )

let run_main () =
  Ast_mapper.run_main run_as_ast_mapper;
  exit 0
