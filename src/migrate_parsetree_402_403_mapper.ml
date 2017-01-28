module From = Ast_402
module To = Ast_403

module A = struct
  include Migrate_parsetree_402_403_migrate
  let copy_cases x = List.map copy_case x
  let copy_expr = copy_expression
  let copy_pat = copy_pattern
  let copy_typ = copy_core_type
end
module B = struct
  include Migrate_parsetree_403_402_migrate
  let copy_cases x = List.map copy_case x
  let copy_expr = copy_expression
  let copy_pat = copy_pattern
  let copy_typ = copy_core_type
end

(*$define fields
  "attribute" "attributes" "case" "cases" "class_declaration"
  "class_description" "class_expr" "class_field" "class_signature"
  "class_structure" "class_type" "class_type_declaration" "class_type_field"
  "constructor_declaration" "expr" "extension" "extension_constructor"
  "include_declaration" "include_description" "label_declaration" "location"
  "module_binding" "module_declaration" "module_expr" "module_type"
  "module_type_declaration" "open_description" "pat" "signature"
  "signature_item" "structure" "structure_item" "typ" "type_declaration"
  "type_extension" "type_kind" "value_binding" "value_description"
  "with_constraint"
*)

let copy_mapper = fun
  ({ From.Ast_mapper.
     (*$foreach field fields field "; "*)
     payload
   } as mapper) ->
  {
    To.Ast_mapper.
    (*$foreach field fields
      field " = (fun _ x -> A.copy_"field" ("field" mapper (B.copy_"field" x)));"*)
    payload = (fun _ x -> A.copy_payload
                  (payload mapper (B.copy_payload Location.none x)))
  }

