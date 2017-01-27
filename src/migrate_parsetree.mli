module Def = Migrate_parsetree_def
module Convert = Migrate_parsetree_convert

type ast =
  | Intf : (module Convert.OCaml_version with type Ast.Parsetree.structure = 'concrete) * 'concrete -> ast
  | Impl : (module Convert.OCaml_version with type Ast.Parsetree.signature = 'concrete) * 'concrete -> ast

(** A simple alias used for the filename of the source that produced an AST *)
type filename = string

type read_error =
  | Not_a_binary_ast of string
  (** The input doesn't contain a binary AST. The argument corresponds
      to the bytes from the input that were consumed. *)
  | Unknown_version of string
  (** The input contains a binary AST for an unknown version of OCaml.
      The argument is the unknown magic number. *)

(** Load a marshalled AST from a channel

    Any exception raised during unmarshalling (see [Marshal]) can escape.  *)
val from_channel : in_channel -> (filename * ast, read_error) result

(** Load a marshalled AST from a byte string.

    See [from_channel] description for exception that can be raised. *)
val from_bytes : bytes -> int -> (filename * ast, read_error) result

(** Marshal an AST to a channel *)
val to_channel : out_channel -> filename -> ast -> unit

(** Marshal an AST to a byte string *)
val to_bytes : filename -> ast -> bytes

(** [missing_feature_description x] is a text describing the feature [x]. *)
val missing_feature_description : Def.missing_feature -> string

(** [missing_feature_minimal_version x] is the OCaml version where x was
    introduced. *)
val missing_feature_minimal_version : Def.missing_feature -> string

(** Turn a missing feature into a reasonable error message. *)
val migration_error_message : Def.missing_feature -> string

module Migrate_402_403 = Migrate_parsetree_402_403
module Migrate_403_402 = Migrate_parsetree_403_402
module Migrate_403_404 = Migrate_parsetree_403_404
module Migrate_404_403 = Migrate_parsetree_404_403
module Migrate_404_405 = Migrate_parsetree_404_405
module Migrate_405_404 = Migrate_parsetree_405_404

(** An alias to the ast version of the current compiler *)
module OCaml_current = Convert.OCaml_OCAML_VERSION
