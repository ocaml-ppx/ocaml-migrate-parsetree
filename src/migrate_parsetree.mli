open Result
module Def = Migrate_parsetree_def

(** Union of all supported versions *)

type ocaml_version = Def.ocaml_version =
  | OCaml_402
  | OCaml_403
  | OCaml_404
  | OCaml_405

val string_of_ocaml_version : ocaml_version -> string

type ('intf, 'impl) intf_or_impl =
  ('intf, 'impl) Migrate_parsetree_def.intf_or_impl =
  | Intf of 'intf
  | Impl of 'impl

type ast =
    Ast_402 of Ast_402.ast
  | Ast_403 of Ast_403.ast
  | Ast_404 of Ast_404.ast
  | Ast_405 of Ast_405.ast

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

open Migrate_parsetree_def

(** Get the OCaml version of an AST *)
val ast_version : ast -> ocaml_version

(** Migrate an AST to another version.

    Migration functions can raise [Migrate_parsetree_def.Migration_error]
    if a feature is not implemented in the targetted version.  *)
val migrate_to_version : ast -> ocaml_version -> ast

(** Migrate any version of an AST to 4.02 *)
val migrate_to_402 : ast -> Ast_402.ast

(** Migrate any version of an AST to 4.03 *)
val migrate_to_403 : ast -> Ast_403.ast

(** Migrate any version of an AST to 4.04 *)
val migrate_to_404 : ast -> Ast_404.ast

(** Migrate any version of an AST to 4.05 *)
val migrate_to_405 : ast -> Ast_405.ast

(** [missing_feature_description x] is a text describing the feature [x]. *)
val missing_feature_description : Def.missing_feature -> string

(** [missing_feature_minimal_version x] is the OCaml version where x was
    introduced. *)
val missing_feature_minimal_version : Def.missing_feature -> ocaml_version

(** Turn a missing feature into a reasonable error message. *)
val migration_error_message : Def.missing_feature -> string

(** Aliases for all modules in the library *)
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

(** An alias to the ast version of the current compiler *)
module Ast_current = Ast_OCAML_VERSION
