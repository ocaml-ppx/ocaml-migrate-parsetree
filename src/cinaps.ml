open StdLabels
open Printf

let nl () = printf "\n"

let parsetree_types =
  [ "structure"
  ; "signature"
  ; "toplevel_phrase"
  ; "core_type"
  ; "expression"
  ; "pattern"
  ; "case"
  ; "type_declaration"
  ; "type_extension"
  ; "extension_constructor"
  ]

let outcometree_types =
  [ "out_value"
  ; "out_type"
  ; "out_class_type"
  ; "out_module_type"
  ; "out_sig_item"
  ; "out_type_extension"
  ; "out_phrase"
  ]

let all_types = parsetree_types @ outcometree_types @ ["mapper"]

let all_types_with_module =
  List.map parsetree_types ~f:(fun t -> ("Parsetree", t)) @
  List.map outcometree_types ~f:(fun t -> ("Outcometree", t)) @
  [("Ast_mapper", "mapper")]

let with_then_and () =
  let first = ref true in
  fun () ->
    if !first then begin
      first := false;
      "with"
    end else
      " and"
