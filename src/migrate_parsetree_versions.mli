(* Abstract view of the Asts of an OCaml frontend *)
(*$define parsetree_types
  "structure" "signature" "toplevel_phrase" "core_type" "expression" "pattern"*)
(*$define outcometree_types
  "out_value" "out_type" "out_class_type" "out_module_type" "out_sig_item"
  "out_type_extension" "out_phrase"*)
(*$define all_types parsetree_types outcometree_types*)

module type Ast = sig
  module Parsetree : sig
    (*$foreach type parsetree_types "type " type "\n"*)
  end
  module Outcometree : sig
    (*$foreach type outcometree_types "type " type "\n"*)
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
    (*$foreach type parsetree_types
         "    " type " : Ast.Parsetree." type ";\n"*)
    (*$foreach type outcometree_types
         "   " type " : Ast.Outcometree." type ";\n"*)
  >
  type _ witnesses += Version : types witnesses
end

(* Supported OCaml versions. *)

module OCaml_402 : OCaml_version with module Ast = Ast_402
module OCaml_403 : OCaml_version with module Ast = Ast_403
module OCaml_404 : OCaml_version with module Ast = Ast_404
module OCaml_405 : OCaml_version with module Ast = Ast_405

(* An alias to the current compiler version *)
module OCaml_current = OCaml_(*$concat OCAML_VERSION*)

val all_versions : (module OCaml_version) list

(* A generic functor converting between any two versions of OCaml. *)

module Convert (A : OCaml_version) (B : OCaml_version) : sig
  (*$foreach type parsetree_types
       "  val copy_" type " : A.Ast.Parsetree." type
                         " -> B.Ast.Parsetree." type "\n"*)
  (*$foreach type outcometree_types
       "  val copy_" type " : A.Ast.Outcometree." type
                         " -> B.Ast.Outcometree." type "\n"*)
end
