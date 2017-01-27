type _ witnesses

module type Ast = sig
  module Parsetree : sig
    type structure
    type signature
    type toplevel_phrase
    type core_type
    type expression
    type pattern
  end
  module Outcometree : sig
    type out_value
    type out_type
    type out_class_type
    type out_module_type
    type out_sig_item
    type out_type_extension
    type out_phrase
  end
  module Config : sig
    val ast_impl_magic_number : string
    val ast_intf_magic_number : string
  end
end

module type OCaml_version = sig
  module Ast : Ast
  type types = <
    core_type : Ast.Parsetree.core_type;
    expression : Ast.Parsetree.expression;
    out_class_type : Ast.Outcometree.out_class_type;
    out_module_type : Ast.Outcometree.out_module_type;
    out_phrase : Ast.Outcometree.out_phrase;
    out_sig_item : Ast.Outcometree.out_sig_item;
    out_type : Ast.Outcometree.out_type;
    out_type_extension : Ast.Outcometree.out_type_extension;
    out_value : Ast.Outcometree.out_value;
    pattern : Ast.Parsetree.pattern;
    signature : Ast.Parsetree.signature;
    structure : Ast.Parsetree.structure;
    toplevel_phrase : Ast.Parsetree.toplevel_phrase
  >
  type _ witnesses += Version : types witnesses
  val string_version : string
end

module OCaml_402 : OCaml_version with module Ast = Ast_402
module OCaml_403 : OCaml_version with module Ast = Ast_403
module OCaml_404 : OCaml_version with module Ast = Ast_404
module OCaml_405 : OCaml_version with module Ast = Ast_405

module Make (A : OCaml_version) (B : OCaml_version) : sig
  val copy_structure :
    A.Ast.Parsetree.structure ->
    B.Ast.Parsetree.structure
  val copy_signature :
    A.Ast.Parsetree.signature ->
    B.Ast.Parsetree.signature
  val copy_toplevel_phrase :
    A.Ast.Parsetree.toplevel_phrase ->
    B.Ast.Parsetree.toplevel_phrase
  val copy_core_type :
    A.Ast.Parsetree.core_type ->
    B.Ast.Parsetree.core_type
  val copy_expression :
    A.Ast.Parsetree.expression ->
    B.Ast.Parsetree.expression
  val copy_pattern :
    A.Ast.Parsetree.pattern ->
    B.Ast.Parsetree.pattern
  val copy_out_value :
    A.Ast.Outcometree.out_value ->
    B.Ast.Outcometree.out_value
  val copy_out_type :
    A.Ast.Outcometree.out_type ->
    B.Ast.Outcometree.out_type
  val copy_out_class_type :
    A.Ast.Outcometree.out_class_type ->
    B.Ast.Outcometree.out_class_type
  val copy_out_module_type :
    A.Ast.Outcometree.out_module_type ->
    B.Ast.Outcometree.out_module_type
  val copy_out_sig_item :
    A.Ast.Outcometree.out_sig_item ->
    B.Ast.Outcometree.out_sig_item
  val copy_out_type_extension :
    A.Ast.Outcometree.out_type_extension ->
    B.Ast.Outcometree.out_type_extension
  val copy_out_phrase :
    A.Ast.Outcometree.out_phrase ->
    B.Ast.Outcometree.out_phrase
end
