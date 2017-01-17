open Result
module Def = Migrate_parsetree_def

type ocaml_version = Migrate_parsetree_def.ocaml_version =
  | OCaml_402
  | OCaml_403
  | OCaml_404
  | OCaml_405

let string_of_ocaml_version = function
  | OCaml_402 -> "4.02"
  | OCaml_403 -> "4.03"
  | OCaml_404 -> "4.04"
  | OCaml_405 -> "4.05"

type ('intf, 'impl) intf_or_impl =
  ('intf, 'impl) Migrate_parsetree_def.intf_or_impl =
  | Intf of 'intf
  | Impl of 'impl

type ast =
  | Ast_402 of Ast_402.ast
  | Ast_403 of Ast_403.ast
  | Ast_404 of Ast_404.ast
  | Ast_405 of Ast_405.ast

type filename = string

let magics = [
  Ast_402.Config.ast_intf_magic_number,
  (fun x -> Ast_402 (Intf (Obj.obj x)));
  Ast_402.Config.ast_impl_magic_number,
  (fun x -> Ast_402 (Impl (Obj.obj x)));
  Ast_403.Config.ast_intf_magic_number,
  (fun x -> Ast_403 (Intf (Obj.obj x)));
  Ast_403.Config.ast_impl_magic_number,
  (fun x -> Ast_403 (Impl (Obj.obj x)));
  Ast_404.Config.ast_intf_magic_number,
  (fun x -> Ast_404 (Intf (Obj.obj x)));
  Ast_404.Config.ast_impl_magic_number,
  (fun x -> Ast_404 (Impl (Obj.obj x)));
  Ast_405.Config.ast_impl_magic_number,
  (fun x -> Ast_405 (Impl (Obj.obj x)));
]

let magic_number = function
  | Ast_402 (Intf _) -> Ast_402.Config.ast_intf_magic_number
  | Ast_402 (Impl _) -> Ast_402.Config.ast_impl_magic_number
  | Ast_403 (Intf _) -> Ast_403.Config.ast_intf_magic_number
  | Ast_403 (Impl _) -> Ast_403.Config.ast_impl_magic_number
  | Ast_404 (Intf _) -> Ast_404.Config.ast_intf_magic_number
  | Ast_404 (Impl _) -> Ast_404.Config.ast_impl_magic_number
  | Ast_405 (Intf _) -> Ast_405.Config.ast_intf_magic_number
  | Ast_405 (Impl _) -> Ast_405.Config.ast_impl_magic_number

let payload = function
  | Ast_402 (Intf x) -> Obj.repr x
  | Ast_402 (Impl x) -> Obj.repr x
  | Ast_403 (Intf x) -> Obj.repr x
  | Ast_403 (Impl x) -> Obj.repr x
  | Ast_404 (Intf x) -> Obj.repr x
  | Ast_404 (Impl x) -> Obj.repr x
  | Ast_405 (Intf x) -> Obj.repr x
  | Ast_405 (Impl x) -> Obj.repr x

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
  match List.assoc magic magics with
  | inj -> Ok inj
  | exception Not_found ->
    let prefix = String.sub magic 0 9 in
    if prefix = String.sub Ast_402.Config.ast_impl_magic_number 0 9 ||
       prefix = String.sub Ast_402.Config.ast_intf_magic_number 0 9 then
      Error (Unknown_version magic)
    else
      Error (Not_a_binary_ast magic)

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

let to_channel oc (filename : filename) x =
  output_string oc (magic_number x);
  output_value oc filename;
  output_value oc (payload x)

let to_bytes (filename : filename) x =
  Bytes.cat (
    Bytes.cat
      (Bytes.of_string (magic_number x))
      (Marshal.to_bytes filename [])
  ) (Marshal.to_bytes (payload x) [])

let ast_version = function
  | Ast_402 _ -> OCaml_402
  | Ast_403 _ -> OCaml_403
  | Ast_404 _ -> OCaml_404
  | Ast_405 _ -> OCaml_405

let down_to_405 = function
  | Ast_405 x -> x
  | _ -> assert false

let down_to_404 = function
  | Ast_404 x -> x
  | x -> match down_to_405 x with
    | Impl x -> Impl (Migrate_parsetree_405_404.copy_structure x)
    | Intf x -> Intf (Migrate_parsetree_405_404.copy_signature x)

let down_to_403 = function
  | Ast_403 x -> x
  | x -> match down_to_404 x with
    | Impl x -> Impl (Migrate_parsetree_404_403.copy_structure x)
    | Intf x -> Intf (Migrate_parsetree_404_403.copy_signature x)

let down_to_402 = function
  | Ast_402 x -> x
  | x -> match down_to_403 x with
    | Impl x -> Impl (Migrate_parsetree_403_402.copy_structure x)
    | Intf x -> Intf (Migrate_parsetree_403_402.copy_signature x)

let up_to_402 = function
  | Ast_402 x -> x
  | _ -> assert false

let up_to_403 = function
  | Ast_403 x -> x
  | x -> match up_to_402 x with
    | Impl x -> Impl (Migrate_parsetree_402_403.copy_structure x)
    | Intf x -> Intf (Migrate_parsetree_402_403.copy_signature x)

let up_to_404 = function
  | Ast_404 x -> x
  | x -> match up_to_403 x with
    | Impl x -> Impl (Migrate_parsetree_403_404.copy_structure x)
    | Intf x -> Intf (Migrate_parsetree_403_404.copy_signature x)

let up_to_405 = function
  | Ast_405 x -> x
  | x -> match up_to_404 x with
    | Impl x -> Impl (Migrate_parsetree_404_405.copy_structure x)
    | Intf x -> Intf (Migrate_parsetree_404_405.copy_signature x)

let migrate_to_402 = down_to_402

let migrate_fun version down_to up_to ast =
  if ast_version ast <= version then up_to ast else down_to ast

let migrate_to_403 ast = migrate_fun OCaml_403 down_to_403 up_to_403 ast
let migrate_to_404 ast = migrate_fun OCaml_404 down_to_404 up_to_404 ast
let migrate_to_405 ast = migrate_fun OCaml_405 down_to_405 up_to_405 ast

let migrate_to_version ast = function
  | OCaml_402 -> Ast_402 (migrate_to_402 ast)
  | OCaml_403 -> Ast_403 (migrate_to_403 ast)
  | OCaml_404 -> Ast_404 (migrate_to_404 ast)
  | OCaml_405 -> Ast_405 (migrate_to_405 ast)

let missing_feature_description = function
  | Def.Pexp_letexception -> "local exceptions"
  | Def.Ppat_open         -> "module open in patterns"
  | Def.Pexp_unreachable  -> "unreachable patterns"
  | Def.PSig              -> "signatures in attribute"
  | Def.Pcstr_record      -> "inline records"
  | Def.Pconst_integer    -> "custom integer literals"
  | Def.Pconst_float      -> "custom float literals"

let missing_feature_minimal_version = function
  | Def.Pexp_letexception -> OCaml_404
  | Def.Ppat_open         -> OCaml_404
  | Def.Pexp_unreachable  -> OCaml_403
  | Def.PSig              -> OCaml_403
  | Def.Pcstr_record      -> OCaml_403
  | Def.Pconst_integer    -> OCaml_403
  | Def.Pconst_float      -> OCaml_403

let migration_error_message x =
  let feature = missing_feature_description x in
  let version = string_of_ocaml_version (missing_feature_minimal_version x) in
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

module Ast_402 = Ast_402
module Ast_403 = Ast_403
module Ast_404 = Ast_404
module Ast_405 = Ast_405
module Migrate_402_403 = Migrate_parsetree_402_403
module Migrate_403_402 = Migrate_parsetree_403_402
module Migrate_403_404 = Migrate_parsetree_403_404
module Migrate_404_403 = Migrate_parsetree_404_403
module Migrate_404_405 = Migrate_parsetree_404_405
module Migrate_405_404 = Migrate_parsetree_405_404

module Ast_current = Ast_OCAML_VERSION

(* Make sure the preprocessing worked as expected *)
let _f (x : Parsetree.expression) : Ast_current.Parsetree.expression = x
