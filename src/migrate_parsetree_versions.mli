(* Abstract view of the Asts of an OCaml frontend *)
(*$ #use "src/cinaps.ml" $*)

module type Ast = sig
  module Parsetree : sig
    (*$ nl (); List.iter parsetree_types ~f:(printf "type %s\n") *)
    type structure
    type signature
    type toplevel_phrase
    type core_type
    type expression
    type pattern
    type case
    type type_declaration
    type type_extension
    type extension_constructor
    (*$*)
  end
  module Outcometree : sig
    (*$ nl (); List.iter outcometree_types ~f:(printf "type %s\n") *)
    type out_value
    type out_type
    type out_class_type
    type out_module_type
    type out_sig_item
    type out_type_extension
    type out_phrase
    (*$*)
  end
  module Ast_mapper : sig
    type mapper
  end
  module Config : sig
    val ast_impl_magic_number : string
    val ast_intf_magic_number : string
  end
end

(* A version of the OCaml frontend packs the ast with type witnesses
   so that equalities can be recovered dynamically. *)
type _ witnesses

module type OCaml_version = sig
  module Ast : Ast
  val string_version : string

  type types = <
    (*$ nl (); List.iter all_types_with_module ~f:(fun (m, s) ->
          printf "    %-21s : Ast.%s.%s;\n" s m s)*)
    structure             : Ast.Parsetree.structure;
    signature             : Ast.Parsetree.signature;
    toplevel_phrase       : Ast.Parsetree.toplevel_phrase;
    core_type             : Ast.Parsetree.core_type;
    expression            : Ast.Parsetree.expression;
    pattern               : Ast.Parsetree.pattern;
    case                  : Ast.Parsetree.case;
    type_declaration      : Ast.Parsetree.type_declaration;
    type_extension        : Ast.Parsetree.type_extension;
    extension_constructor : Ast.Parsetree.extension_constructor;
    out_value             : Ast.Outcometree.out_value;
    out_type              : Ast.Outcometree.out_type;
    out_class_type        : Ast.Outcometree.out_class_type;
    out_module_type       : Ast.Outcometree.out_module_type;
    out_sig_item          : Ast.Outcometree.out_sig_item;
    out_type_extension    : Ast.Outcometree.out_type_extension;
    out_phrase            : Ast.Outcometree.out_phrase;
    mapper                : Ast.Ast_mapper.mapper;
    (*$*)
  >
  type _ witnesses += Version : types witnesses
end

(* Supported OCaml versions. *)

module OCaml_402 : OCaml_version with module Ast = Ast_402
module OCaml_403 : OCaml_version with module Ast = Ast_403
module OCaml_404 : OCaml_version with module Ast = Ast_404
module OCaml_405 : OCaml_version with module Ast = Ast_405

(* An alias to the current compiler version *)
module OCaml_current = OCaml_(*#concat OCAML_VERSION*)

val all_versions : (module OCaml_version) list

(* A generic functor converting between any two versions of OCaml. *)

module Convert (A : OCaml_version) (B : OCaml_version) : sig
  (*$ nl (); List.iter all_types_with_module ~f:(fun (m, s) ->
        let fq = sprintf "%s.%s" m s in
        printf "  val copy_%-21s : A.Ast.%-31s -> B.Ast.%s\n" s fq fq) *)
  val copy_structure             : A.Ast.Parsetree.structure             -> B.Ast.Parsetree.structure
  val copy_signature             : A.Ast.Parsetree.signature             -> B.Ast.Parsetree.signature
  val copy_toplevel_phrase       : A.Ast.Parsetree.toplevel_phrase       -> B.Ast.Parsetree.toplevel_phrase
  val copy_core_type             : A.Ast.Parsetree.core_type             -> B.Ast.Parsetree.core_type
  val copy_expression            : A.Ast.Parsetree.expression            -> B.Ast.Parsetree.expression
  val copy_pattern               : A.Ast.Parsetree.pattern               -> B.Ast.Parsetree.pattern
  val copy_case                  : A.Ast.Parsetree.case                  -> B.Ast.Parsetree.case
  val copy_type_declaration      : A.Ast.Parsetree.type_declaration      -> B.Ast.Parsetree.type_declaration
  val copy_type_extension        : A.Ast.Parsetree.type_extension        -> B.Ast.Parsetree.type_extension
  val copy_extension_constructor : A.Ast.Parsetree.extension_constructor -> B.Ast.Parsetree.extension_constructor
  val copy_out_value             : A.Ast.Outcometree.out_value           -> B.Ast.Outcometree.out_value
  val copy_out_type              : A.Ast.Outcometree.out_type            -> B.Ast.Outcometree.out_type
  val copy_out_class_type        : A.Ast.Outcometree.out_class_type      -> B.Ast.Outcometree.out_class_type
  val copy_out_module_type       : A.Ast.Outcometree.out_module_type     -> B.Ast.Outcometree.out_module_type
  val copy_out_sig_item          : A.Ast.Outcometree.out_sig_item        -> B.Ast.Outcometree.out_sig_item
  val copy_out_type_extension    : A.Ast.Outcometree.out_type_extension  -> B.Ast.Outcometree.out_type_extension
  val copy_out_phrase            : A.Ast.Outcometree.out_phrase          -> B.Ast.Outcometree.out_phrase
  val copy_mapper                : A.Ast.Ast_mapper.mapper               -> B.Ast.Ast_mapper.mapper
  (*$*)
end
