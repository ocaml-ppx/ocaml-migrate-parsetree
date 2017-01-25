module Def = Migrate_parsetree_def
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

type ocaml_version =
  | OCaml_402
  | OCaml_403
  | OCaml_404
  | OCaml_405

let string_of_ocaml_version = function
  | OCaml_402 -> "4.02"
  | OCaml_403 -> "4.03"
  | OCaml_404 -> "4.04"
  | OCaml_405 -> "4.05"

type _ signature =
  | Sig_402 : Ast_402.Parsetree.signature signature
  | Sig_403 : Ast_403.Parsetree.signature signature
  | Sig_404 : Ast_404.Parsetree.signature signature
  | Sig_405 : Ast_405.Parsetree.signature signature

type _ structure =
  | Str_402 : Ast_402.Parsetree.structure structure
  | Str_403 : Ast_403.Parsetree.structure structure
  | Str_404 : Ast_404.Parsetree.structure structure
  | Str_405 : Ast_405.Parsetree.structure structure

type _ toplevel_phrase =
  | Top_402 : Ast_402.Parsetree.toplevel_phrase toplevel_phrase
  | Top_403 : Ast_403.Parsetree.toplevel_phrase toplevel_phrase
  | Top_404 : Ast_404.Parsetree.toplevel_phrase toplevel_phrase
  | Top_405 : Ast_405.Parsetree.toplevel_phrase toplevel_phrase

type _ out_phrase =
  | Out_402 : Ast_402.Outcometree.out_phrase out_phrase
  | Out_403 : Ast_403.Outcometree.out_phrase out_phrase
  | Out_404 : Ast_404.Outcometree.out_phrase out_phrase
  | Out_405 : Ast_405.Outcometree.out_phrase out_phrase

type ast =
  | Intf : 'concrete signature * 'concrete -> ast
  | Impl : 'concrete structure * 'concrete -> ast

type filename = string

let magics = [
  Ast_402.Config.ast_intf_magic_number, (fun x -> Intf (Sig_402, Obj.obj x));
  Ast_402.Config.ast_impl_magic_number, (fun x -> Impl (Str_402, Obj.obj x));
  Ast_403.Config.ast_intf_magic_number, (fun x -> Intf (Sig_403, Obj.obj x));
  Ast_403.Config.ast_impl_magic_number, (fun x -> Impl (Str_403, Obj.obj x));
  Ast_404.Config.ast_intf_magic_number, (fun x -> Intf (Sig_404, Obj.obj x));
  Ast_404.Config.ast_impl_magic_number, (fun x -> Impl (Str_404, Obj.obj x));
  Ast_405.Config.ast_intf_magic_number, (fun x -> Intf (Sig_404, Obj.obj x));
  Ast_405.Config.ast_impl_magic_number, (fun x -> Impl (Str_405, Obj.obj x));
]

let magic_number = function
  | Intf (Sig_402, _) -> Ast_402.Config.ast_intf_magic_number
  | Impl (Str_402, _) -> Ast_402.Config.ast_impl_magic_number
  | Intf (Sig_403, _) -> Ast_403.Config.ast_intf_magic_number
  | Impl (Str_403, _) -> Ast_403.Config.ast_impl_magic_number
  | Intf (Sig_404, _) -> Ast_404.Config.ast_intf_magic_number
  | Impl (Str_404, _) -> Ast_404.Config.ast_impl_magic_number
  | Intf (Sig_405, _) -> Ast_405.Config.ast_intf_magic_number
  | Impl (Str_405, _) -> Ast_405.Config.ast_impl_magic_number

let payload = function
  | Intf (_, x) -> Obj.repr x
  | Impl (_, x) -> Obj.repr x

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
  | Impl (Str_402, _) -> OCaml_402
  | Impl (Str_403, _) -> OCaml_403
  | Impl (Str_404, _) -> OCaml_404
  | Impl (Str_405, _) -> OCaml_405
  | Intf (Sig_402, _) -> OCaml_402
  | Intf (Sig_403, _) -> OCaml_403
  | Intf (Sig_404, _) -> OCaml_404
  | Intf (Sig_405, _) -> OCaml_405

let migrate_objects (i_ver : _ -> ocaml_version) (o_ver : _ -> ocaml_version) =
  fun down up i_v i o_v ->
    if i_ver i_v < o_ver o_v then up i_v i o_v else down i_v i o_v

let rec sig_down : type i o. i signature -> i -> o signature -> o =
  fun (type i) (type o) (i_v : i signature) (i : i) (o_v : o signature) ->
    match i_v, o_v with
    | Sig_405, Sig_405 -> (i : o)
    | Sig_404, Sig_404 -> (i : o)
    | Sig_403, Sig_403 -> (i : o)
    | Sig_402, Sig_402 -> (i : o)
    | Sig_405, _ -> sig_down Sig_404 (Migrate_405_404.copy_signature i) o_v
    | Sig_404, _ -> sig_down Sig_403 (Migrate_404_403.copy_signature i) o_v
    | Sig_403, _ -> sig_down Sig_402 (Migrate_403_402.copy_signature i) o_v
    | Sig_402, _ -> assert false

let rec sig_up : type i o. i signature -> i -> o signature -> o =
  fun (type i) (type o) (i_v : i signature) (i : i) (o_v : o signature) ->
    match i_v, o_v with
    | Sig_405, Sig_405 -> (i : o)
    | Sig_404, Sig_404 -> (i : o)
    | Sig_403, Sig_403 -> (i : o)
    | Sig_402, Sig_402 -> (i : o)
    | Sig_402, _ -> sig_up Sig_403 (Migrate_402_403.copy_signature i) o_v
    | Sig_403, _ -> sig_up Sig_404 (Migrate_403_404.copy_signature i) o_v
    | Sig_404, _ -> sig_up Sig_405 (Migrate_404_405.copy_signature i) o_v
    | Sig_405, _ -> assert false

let signature_version (type a) (x : a signature) = match x with
  | Sig_402 -> OCaml_402
  | Sig_403 -> OCaml_403
  | Sig_404 -> OCaml_404
  | Sig_405 -> OCaml_405

let signature_migrate i_v i o_v =
  migrate_objects signature_version signature_version sig_down sig_up i_v i o_v

let rec str_down : type i o. i structure -> i -> o structure -> o =
  fun (type i) (type o) (i_v : i structure) (i : i) (o_v : o structure) ->
    match i_v, o_v with
    | Str_405, Str_405 -> (i : o)
    | Str_404, Str_404 -> (i : o)
    | Str_403, Str_403 -> (i : o)
    | Str_402, Str_402 -> (i : o)
    | Str_405, _ -> str_down Str_404 (Migrate_405_404.copy_structure i) o_v
    | Str_404, _ -> str_down Str_403 (Migrate_404_403.copy_structure i) o_v
    | Str_403, _ -> str_down Str_402 (Migrate_403_402.copy_structure i) o_v
    | Str_402, _ -> assert false

let rec str_up : type i o. i structure -> i -> o structure -> o =
  fun (type i) (type o) (i_v : i structure) (i : i) (o_v : o structure) ->
    match i_v, o_v with
    | Str_405, Str_405 -> (i : o)
    | Str_404, Str_404 -> (i : o)
    | Str_403, Str_403 -> (i : o)
    | Str_402, Str_402 -> (i : o)
    | Str_402, _ -> str_up Str_403 (Migrate_402_403.copy_structure i) o_v
    | Str_403, _ -> str_up Str_404 (Migrate_403_404.copy_structure i) o_v
    | Str_404, _ -> str_up Str_405 (Migrate_404_405.copy_structure i) o_v
    | Str_405, _ -> assert false

let structure_version (type a) (x : a structure) = match x with
  | Str_402 -> OCaml_402
  | Str_403 -> OCaml_403
  | Str_404 -> OCaml_404
  | Str_405 -> OCaml_405

let structure_migrate i_v i o_v =
  migrate_objects structure_version structure_version str_down str_up i_v i o_v

let rec top_down : type i o. i toplevel_phrase -> i -> o toplevel_phrase -> o =
  fun (type i) (type o) (i_v : i toplevel_phrase) (i : i) (o_v : o toplevel_phrase) ->
    match i_v, o_v with
    | Top_405, Top_405 -> (i : o)
    | Top_404, Top_404 -> (i : o)
    | Top_403, Top_403 -> (i : o)
    | Top_402, Top_402 -> (i : o)
    | Top_405, _ -> top_down Top_404 (Migrate_405_404.copy_toplevel_phrase i) o_v
    | Top_404, _ -> top_down Top_403 (Migrate_404_403.copy_toplevel_phrase i) o_v
    | Top_403, _ -> top_down Top_402 (Migrate_403_402.copy_toplevel_phrase i) o_v
    | Top_402, _ -> assert false

let rec top_up : type i o. i toplevel_phrase -> i -> o toplevel_phrase -> o =
  fun (type i) (type o) (i_v : i toplevel_phrase) (i : i) (o_v : o toplevel_phrase) ->
    match i_v, o_v with
    | Top_405, Top_405 -> (i : o)
    | Top_404, Top_404 -> (i : o)
    | Top_403, Top_403 -> (i : o)
    | Top_402, Top_402 -> (i : o)
    | Top_402, _ -> top_up Top_403 (Migrate_402_403.copy_toplevel_phrase i) o_v
    | Top_403, _ -> top_up Top_404 (Migrate_403_404.copy_toplevel_phrase i) o_v
    | Top_404, _ -> top_up Top_405 (Migrate_404_405.copy_toplevel_phrase i) o_v
    | Top_405, _ -> assert false

let toplevel_phrase_version (type a) (x : a toplevel_phrase) = match x with
  | Top_402 -> OCaml_402
  | Top_403 -> OCaml_403
  | Top_404 -> OCaml_404
  | Top_405 -> OCaml_405

let toplevel_phrase_migrate i_v i o_v =
  migrate_objects toplevel_phrase_version toplevel_phrase_version
    top_down top_up i_v i o_v

let rec out_down : type i o. i out_phrase -> i -> o out_phrase -> o =
  fun (type i) (type o) (i_v : i out_phrase) (i : i) (o_v : o out_phrase) ->
    match i_v, o_v with
    | Out_405, Out_405 -> (i : o)
    | Out_404, Out_404 -> (i : o)
    | Out_403, Out_403 -> (i : o)
    | Out_402, Out_402 -> (i : o)
    | Out_405, _ -> out_down Out_404 (Migrate_405_404.copy_out_phrase i) o_v
    | Out_404, _ -> out_down Out_403 (Migrate_404_403.copy_out_phrase i) o_v
    | Out_403, _ -> out_down Out_402 (Migrate_403_402.copy_out_phrase i) o_v
    | Out_402, _ -> assert false

let rec out_up : type i o. i out_phrase -> i -> o out_phrase -> o =
  fun (type i) (type o) (i_v : i out_phrase) (i : i) (o_v : o out_phrase) ->
    match i_v, o_v with
    | Out_405, Out_405 -> (i : o)
    | Out_404, Out_404 -> (i : o)
    | Out_403, Out_403 -> (i : o)
    | Out_402, Out_402 -> (i : o)
    | Out_402, _ -> out_up Out_403 (Migrate_402_403.copy_out_phrase i) o_v
    | Out_403, _ -> out_up Out_404 (Migrate_403_404.copy_out_phrase i) o_v
    | Out_404, _ -> out_up Out_405 (Migrate_404_405.copy_out_phrase i) o_v
    | Out_405, _ -> assert false

let out_phrase_version (type a) (x : a out_phrase) = match x with
  | Out_402 -> OCaml_402
  | Out_403 -> OCaml_403
  | Out_404 -> OCaml_404
  | Out_405 -> OCaml_405

let out_phrase_migrate i_v i o_v =
  migrate_objects out_phrase_version out_phrase_version
    out_down out_up i_v i o_v

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

module OCaml_402 = struct
  module Ast = Ast_402
  let version = OCaml_402
  let signature = Sig_402
  let structure = Str_402
  let toplevel_phrase = Top_402
  let out_phrase = Out_402
end

module OCaml_403 = struct
  module Ast = Ast_403
  let version = OCaml_403
  let signature = Sig_403
  let structure = Str_403
  let toplevel_phrase = Top_403
  let out_phrase = Out_403
end

module OCaml_404 = struct
  module Ast = Ast_404
  let version = OCaml_404
  let signature = Sig_404
  let structure = Str_404
  let toplevel_phrase = Top_404
  let out_phrase = Out_404
end

module OCaml_405 = struct
  module Ast = Ast_405
  let version = OCaml_405
  let signature = Sig_405
  let structure = Str_405
  let toplevel_phrase = Top_405
  let out_phrase = Out_405
end

module OCaml_current = OCaml_OCAML_VERSION

(* Make sure the preprocessing worked as expected *)
let _f (x : Parsetree.expression) : OCaml_current.Ast.Parsetree.expression = x
