module Def = Migrate_parsetree_def
module Ast_402 = Ast_402
module Ast_403 = Ast_403
module Ast_404 = Ast_404
module Ast_405 = Ast_405
module Convert = Migrate_parsetree_convert
module Migrate_402_403 = Migrate_parsetree_402_403
module Migrate_403_402 = Migrate_parsetree_403_402
module Migrate_403_404 = Migrate_parsetree_403_404
module Migrate_404_403 = Migrate_parsetree_404_403
module Migrate_404_405 = Migrate_parsetree_404_405
module Migrate_405_404 = Migrate_parsetree_405_404

type ocaml_version = (module Convert.OCaml_version)

type ast =
  | Intf : (module Convert.OCaml_version with type Ast.Parsetree.structure = 'concrete) * 'concrete -> ast
  | Impl : (module Convert.OCaml_version with type Ast.Parsetree.signature = 'concrete) * 'concrete -> ast

type filename = string

let all_versions : ocaml_version list = [
  (module Convert.OCaml_402 : Convert.OCaml_version);
  (module Convert.OCaml_403 : Convert.OCaml_version);
  (module Convert.OCaml_404 : Convert.OCaml_version);
  (module Convert.OCaml_405 : Convert.OCaml_version);
]

let magic_length = String.length Ast_402.Config.ast_impl_magic_number

let read_magic ic =
  let buf = Bytes.create magic_length in
  let len = input ic buf 0 magic_length in
  let s = Bytes.sub_string buf 0 len in
  if len = magic_length then
    Ok s
  else
    Error s

type read_error =
  | Not_a_binary_ast of string
  | Unknown_version of string

let find_magic magic =
  let rec loop = function
    | [] ->
        let prefix = String.sub magic 0 9 in
        if prefix = String.sub Ast_402.Config.ast_impl_magic_number 0 9 ||
           prefix = String.sub Ast_402.Config.ast_intf_magic_number 0 9 then
          Error (Unknown_version magic)
        else
          Error (Not_a_binary_ast magic)
    | (module Frontend : Convert.OCaml_version) :: tail ->
        if Frontend.Ast.Config.ast_impl_magic_number = magic then
          Ok (fun x -> Impl ((module Frontend), Obj.obj x))
        else if Frontend.Ast.Config.ast_intf_magic_number = magic then
          Ok (fun x -> Intf ((module Frontend), Obj.obj x))
        else
          loop tail
  in
  loop all_versions

let from_channel ic =
  match read_magic ic with
  | Error s -> Error (Not_a_binary_ast s)
  | Ok s ->
    match find_magic s with
    | Ok inj ->
      let filename : filename = input_value ic in
      let payload = inj (input_value ic) in
      Ok (filename, payload)
    | Error _ as e  -> e

let from_bytes bytes pos =
  if Bytes.length bytes - pos < magic_length then
    Error (Not_a_binary_ast "")
  else
    let magic = Bytes.to_string (Bytes.sub bytes pos magic_length) in
    match find_magic magic with
    | Ok inj ->
      let filename_pos = pos + magic_length in
      let filename : filename = Marshal.from_bytes bytes filename_pos in
      let payload_pos = filename_pos + Marshal.total_size bytes filename_pos in
      let payload = inj (Marshal.from_bytes bytes payload_pos) in
      Ok (filename, payload)
    | Error _ as e -> e

let decompose_ast = function
  | Impl ((module Frontend), tree) ->
      (Frontend.Ast.Config.ast_impl_magic_number, Obj.repr tree)
  | Intf ((module Frontend), tree) ->
      (Frontend.Ast.Config.ast_intf_magic_number, Obj.repr tree)

let to_channel oc (filename : filename) x =
  let magic_number, payload = decompose_ast x in
  output_string oc magic_number;
  output_value oc filename;
  output_value oc payload

let to_bytes (filename : filename) x =
  let magic_number, payload = decompose_ast x in
  Bytes.cat (
    Bytes.cat
      (Bytes.of_string magic_number)
      (Marshal.to_bytes filename [])
  ) (Marshal.to_bytes payload [])

let missing_feature_description = function
  | Def.Pexp_letexception -> "local exceptions"
  | Def.Ppat_open         -> "module open in patterns"
  | Def.Pexp_unreachable  -> "unreachable patterns"
  | Def.PSig              -> "signatures in attribute"
  | Def.Pcstr_record      -> "inline records"
  | Def.Pconst_integer    -> "custom integer literals"
  | Def.Pconst_float      -> "custom float literals"

let missing_feature_minimal_version = function
  | Def.Pexp_letexception -> "OCaml 4.04"
  | Def.Ppat_open         -> "OCaml 4.04"
  | Def.Pexp_unreachable  -> "OCaml 4.03"
  | Def.PSig              -> "OCaml 4.03"
  | Def.Pcstr_record      -> "OCaml 4.03"
  | Def.Pconst_integer    -> "OCaml 4.03"
  | Def.Pconst_float      -> "OCaml 4.03"

let migration_error_message x =
  let feature = missing_feature_description x in
  let version = missing_feature_minimal_version x in
  feature ^ " are not supported before OCaml " ^ version

let location_prefix l =
  if l = Location.none then "" else
    let {Location.loc_start; loc_end; _} = l in
    let bol = loc_start.Lexing.pos_bol in
    Printf.sprintf "File %S, line %d, characters %d-%d: "
      loc_start.Lexing.pos_fname
      loc_start.Lexing.pos_lnum
      (loc_start.Lexing.pos_cnum - bol)
      (loc_end.Lexing.pos_cnum - bol)

let () = Printexc.register_printer (function
    | Def.Migration_error (err, loc) ->
        Some (location_prefix loc ^ migration_error_message err)
    | _ -> None
  )

module OCaml_current = Convert.OCaml_OCAML_VERSION

(* Make sure the preprocessing worked as expected *)
let _f (x : Parsetree.expression) : OCaml_current.Ast.Parsetree.expression = x
