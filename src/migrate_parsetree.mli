module Def = Migrate_parsetree_def

(** Union of all supported versions *)

type ocaml_version =
  | OCaml_402
  | OCaml_403
  | OCaml_404
  | OCaml_405

val string_of_ocaml_version : ocaml_version -> string

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

(** Get the OCaml version *)
val signature_version : _ signature -> ocaml_version
val structure_version : _ structure -> ocaml_version
val toplevel_phrase_version : _ toplevel_phrase -> ocaml_version
val out_phrase_version : _ out_phrase -> ocaml_version
val ast_version : ast -> ocaml_version

(** Migrate an AST to another version.

    Migration functions can raise [Migrate_parsetree_def.Migration_error]
    if a feature is not implemented in the targetted version.  *)
val signature_migrate : 'input signature -> 'input -> 'output signature -> 'output
val structure_migrate : 'input structure -> 'input -> 'output structure -> 'output
val toplevel_phrase_migrate : 'input toplevel_phrase -> 'input -> 'output toplevel_phrase -> 'output
val out_phrase_migrate : 'input out_phrase -> 'input -> 'output out_phrase -> 'output

(** [missing_feature_description x] is a text describing the feature [x]. *)
val missing_feature_description : Def.missing_feature -> string

(** [missing_feature_minimal_version x] is the OCaml version where x was
    introduced. *)
val missing_feature_minimal_version : Def.missing_feature -> ocaml_version

(** Turn a missing feature into a reasonable error message. *)
val migration_error_message : Def.missing_feature -> string

(** Aliases for all modules in the library *)
module OCaml_402 : sig
  module Ast = Ast_402
  val version : ocaml_version
  val signature : Ast.Parsetree.signature signature
  val structure : Ast.Parsetree.structure structure
  val toplevel_phrase : Ast.Parsetree.toplevel_phrase toplevel_phrase
  val out_phrase : Ast.Outcometree.out_phrase out_phrase
end

module OCaml_403 : sig
  module Ast = Ast_403
  val version : ocaml_version
  val signature : Ast.Parsetree.signature signature
  val structure : Ast.Parsetree.structure structure
  val toplevel_phrase : Ast.Parsetree.toplevel_phrase toplevel_phrase
  val out_phrase : Ast.Outcometree.out_phrase out_phrase
end

module OCaml_404 : sig
  module Ast = Ast_404
  val version : ocaml_version
  val signature : Ast.Parsetree.signature signature
  val structure : Ast.Parsetree.structure structure
  val toplevel_phrase : Ast.Parsetree.toplevel_phrase toplevel_phrase
  val out_phrase : Ast.Outcometree.out_phrase out_phrase
end

module OCaml_405 : sig
  module Ast = Ast_405
  val version : ocaml_version
  val signature : Ast.Parsetree.signature signature
  val structure : Ast.Parsetree.structure structure
  val toplevel_phrase : Ast.Parsetree.toplevel_phrase toplevel_phrase
  val out_phrase : Ast.Outcometree.out_phrase out_phrase
end

module Migrate_402_403 = Migrate_parsetree_402_403
module Migrate_403_402 = Migrate_parsetree_403_402
module Migrate_403_404 = Migrate_parsetree_403_404
module Migrate_404_403 = Migrate_parsetree_404_403
module Migrate_404_405 = Migrate_parsetree_404_405
module Migrate_405_404 = Migrate_parsetree_405_404

(** An alias to the ast version of the current compiler *)
module OCaml_current = OCaml_OCAML_VERSION
