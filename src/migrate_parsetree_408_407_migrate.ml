module From = Ast_408
module To = Ast_407

module Def = Migrate_parsetree_def

let migration_error location feature =
  raise (Def.Migration_error (feature, location))

let rec copy_Ast_408_Parsetree_toplevel_phrase :
  Ast_408.Parsetree.toplevel_phrase -> Ast_407.Parsetree.toplevel_phrase =
  function
  | Ast_408.Parsetree.Ptop_def x0 ->
      Ast_407.Parsetree.Ptop_def (copy_Ast_408_Parsetree_structure x0)
  | Ast_408.Parsetree.Ptop_dir
      { Ast_408.Parsetree.pdir_name;
        Ast_408.Parsetree.pdir_arg;
        Ast_408.Parsetree.pdir_loc = _; } ->
      Ast_407.Parsetree.Ptop_dir
        (pdir_name.Location.txt,
         (match pdir_arg with
          | None -> Ast_407.Parsetree.Pdir_none
          | Some arg -> copy_Ast_408_Parsetree_directive_argument arg))

and copy_Ast_408_Parsetree_directive_argument :
  Ast_408.Parsetree.directive_argument ->
    Ast_407.Parsetree.directive_argument
  =
  fun
    { Ast_408.Parsetree.pdira_desc = pdira_desc;
      Ast_408.Parsetree.pdira_loc = _pdira_loc }
     ->
       (copy_Ast_408_Parsetree_directive_argument_desc pdira_desc)

and copy_Ast_408_Parsetree_directive_argument_desc :
  Ast_408.Parsetree.directive_argument_desc ->
    Ast_407.Parsetree.directive_argument
  =
  function
  | Ast_408.Parsetree.Pdir_string x0 -> Ast_407.Parsetree.Pdir_string x0
  | Ast_408.Parsetree.Pdir_int (x0,x1) ->
      Ast_407.Parsetree.Pdir_int (x0, (copy_option (fun x  -> x) x1))
  | Ast_408.Parsetree.Pdir_ident x0 ->
      Ast_407.Parsetree.Pdir_ident (copy_Ast_408_Longident_t x0)
  | Ast_408.Parsetree.Pdir_bool x0 ->
      Ast_407.Parsetree.Pdir_bool (copy_bool x0)

and copy_Ast_408_Parsetree_expression :
  Ast_408.Parsetree.expression -> Ast_407.Parsetree.expression =
  fun
    { Ast_408.Parsetree.pexp_desc = pexp_desc;
      Ast_408.Parsetree.pexp_loc = pexp_loc;
      Ast_408.Parsetree.pexp_loc_stack = _;
      Ast_408.Parsetree.pexp_attributes = pexp_attributes }
     ->
    {
      Ast_407.Parsetree.pexp_desc =
        (copy_Ast_408_Parsetree_expression_desc pexp_desc);
      Ast_407.Parsetree.pexp_loc = (copy_Ast_408_Location_t pexp_loc);
      Ast_407.Parsetree.pexp_attributes =
        (copy_Ast_408_Parsetree_attributes pexp_attributes)
    }

and copy_Ast_408_Parsetree_expression_desc :
  Ast_408.Parsetree.expression_desc -> Ast_407.Parsetree.expression_desc =
  function
  | Ast_408.Parsetree.Pexp_ident x0 ->
      Ast_407.Parsetree.Pexp_ident
        (copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0)
  | Ast_408.Parsetree.Pexp_constant x0 ->
      Ast_407.Parsetree.Pexp_constant (copy_Ast_408_Parsetree_constant x0)
  | Ast_408.Parsetree.Pexp_let (x0,x1,x2) ->
      Ast_407.Parsetree.Pexp_let
        ((copy_Ast_408_Asttypes_rec_flag x0),
          (List.map copy_Ast_408_Parsetree_value_binding x1),
          (copy_Ast_408_Parsetree_expression x2))
  | Ast_408.Parsetree.Pexp_function x0 ->
      Ast_407.Parsetree.Pexp_function
        (List.map copy_Ast_408_Parsetree_case x0)
  | Ast_408.Parsetree.Pexp_fun (x0,x1,x2,x3) ->
      Ast_407.Parsetree.Pexp_fun
        ((copy_Ast_408_Asttypes_arg_label x0),
          (copy_option copy_Ast_408_Parsetree_expression x1),
          (copy_Ast_408_Parsetree_pattern x2),
          (copy_Ast_408_Parsetree_expression x3))
  | Ast_408.Parsetree.Pexp_apply (x0,x1) ->
      Ast_407.Parsetree.Pexp_apply
        ((copy_Ast_408_Parsetree_expression x0),
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_Ast_408_Asttypes_arg_label x0),
                  (copy_Ast_408_Parsetree_expression x1))) x1))
  | Ast_408.Parsetree.Pexp_match (x0,x1) ->
      Ast_407.Parsetree.Pexp_match
        ((copy_Ast_408_Parsetree_expression x0),
          (List.map copy_Ast_408_Parsetree_case x1))
  | Ast_408.Parsetree.Pexp_try (x0,x1) ->
      Ast_407.Parsetree.Pexp_try
        ((copy_Ast_408_Parsetree_expression x0),
          (List.map copy_Ast_408_Parsetree_case x1))
  | Ast_408.Parsetree.Pexp_tuple x0 ->
      Ast_407.Parsetree.Pexp_tuple
        (List.map copy_Ast_408_Parsetree_expression x0)
  | Ast_408.Parsetree.Pexp_construct (x0,x1) ->
      Ast_407.Parsetree.Pexp_construct
        ((copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0),
          (copy_option copy_Ast_408_Parsetree_expression x1))
  | Ast_408.Parsetree.Pexp_variant (x0,x1) ->
      Ast_407.Parsetree.Pexp_variant
        ((copy_Ast_408_Asttypes_label x0),
          (copy_option copy_Ast_408_Parsetree_expression x1))
  | Ast_408.Parsetree.Pexp_record (x0,x1) ->
      Ast_407.Parsetree.Pexp_record
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               ((copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0),
                 (copy_Ast_408_Parsetree_expression x1))) x0),
          (copy_option copy_Ast_408_Parsetree_expression x1))
  | Ast_408.Parsetree.Pexp_field (x0,x1) ->
      Ast_407.Parsetree.Pexp_field
        ((copy_Ast_408_Parsetree_expression x0),
          (copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x1))
  | Ast_408.Parsetree.Pexp_setfield (x0,x1,x2) ->
      Ast_407.Parsetree.Pexp_setfield
        ((copy_Ast_408_Parsetree_expression x0),
          (copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x1),
          (copy_Ast_408_Parsetree_expression x2))
  | Ast_408.Parsetree.Pexp_array x0 ->
      Ast_407.Parsetree.Pexp_array
        (List.map copy_Ast_408_Parsetree_expression x0)
  | Ast_408.Parsetree.Pexp_ifthenelse (x0,x1,x2) ->
      Ast_407.Parsetree.Pexp_ifthenelse
        ((copy_Ast_408_Parsetree_expression x0),
          (copy_Ast_408_Parsetree_expression x1),
          (copy_option copy_Ast_408_Parsetree_expression x2))
  | Ast_408.Parsetree.Pexp_sequence (x0,x1) ->
      Ast_407.Parsetree.Pexp_sequence
        ((copy_Ast_408_Parsetree_expression x0),
          (copy_Ast_408_Parsetree_expression x1))
  | Ast_408.Parsetree.Pexp_while (x0,x1) ->
      Ast_407.Parsetree.Pexp_while
        ((copy_Ast_408_Parsetree_expression x0),
          (copy_Ast_408_Parsetree_expression x1))
  | Ast_408.Parsetree.Pexp_for (x0,x1,x2,x3,x4) ->
      Ast_407.Parsetree.Pexp_for
        ((copy_Ast_408_Parsetree_pattern x0),
          (copy_Ast_408_Parsetree_expression x1),
          (copy_Ast_408_Parsetree_expression x2),
          (copy_Ast_408_Asttypes_direction_flag x3),
          (copy_Ast_408_Parsetree_expression x4))
  | Ast_408.Parsetree.Pexp_constraint (x0,x1) ->
      Ast_407.Parsetree.Pexp_constraint
        ((copy_Ast_408_Parsetree_expression x0),
          (copy_Ast_408_Parsetree_core_type x1))
  | Ast_408.Parsetree.Pexp_coerce (x0,x1,x2) ->
      Ast_407.Parsetree.Pexp_coerce
        ((copy_Ast_408_Parsetree_expression x0),
          (copy_option copy_Ast_408_Parsetree_core_type x1),
          (copy_Ast_408_Parsetree_core_type x2))
  | Ast_408.Parsetree.Pexp_send (x0,x1) ->
      Ast_407.Parsetree.Pexp_send
        ((copy_Ast_408_Parsetree_expression x0),
          (copy_Ast_408_Asttypes_loc copy_Ast_408_Asttypes_label x1))
  | Ast_408.Parsetree.Pexp_new x0 ->
      Ast_407.Parsetree.Pexp_new
        (copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0)
  | Ast_408.Parsetree.Pexp_setinstvar (x0,x1) ->
      Ast_407.Parsetree.Pexp_setinstvar
        ((copy_Ast_408_Asttypes_loc copy_Ast_408_Asttypes_label x0),
          (copy_Ast_408_Parsetree_expression x1))
  | Ast_408.Parsetree.Pexp_override x0 ->
      Ast_407.Parsetree.Pexp_override
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_Ast_408_Asttypes_loc copy_Ast_408_Asttypes_label x0),
                (copy_Ast_408_Parsetree_expression x1))) x0)
  | Ast_408.Parsetree.Pexp_letmodule (x0,x1,x2) ->
      Ast_407.Parsetree.Pexp_letmodule
        ((copy_Ast_408_Asttypes_loc (fun x  -> x) x0),
          (copy_Ast_408_Parsetree_module_expr x1),
          (copy_Ast_408_Parsetree_expression x2))
  | Ast_408.Parsetree.Pexp_letexception (x0,x1) ->
      Ast_407.Parsetree.Pexp_letexception
        ((copy_Ast_408_Parsetree_extension_constructor x0),
          (copy_Ast_408_Parsetree_expression x1))
  | Ast_408.Parsetree.Pexp_assert x0 ->
      Ast_407.Parsetree.Pexp_assert (copy_Ast_408_Parsetree_expression x0)
  | Ast_408.Parsetree.Pexp_lazy x0 ->
      Ast_407.Parsetree.Pexp_lazy (copy_Ast_408_Parsetree_expression x0)
  | Ast_408.Parsetree.Pexp_poly (x0,x1) ->
      Ast_407.Parsetree.Pexp_poly
        ((copy_Ast_408_Parsetree_expression x0),
          (copy_option copy_Ast_408_Parsetree_core_type x1))
  | Ast_408.Parsetree.Pexp_object x0 ->
      Ast_407.Parsetree.Pexp_object
        (copy_Ast_408_Parsetree_class_structure x0)
  | Ast_408.Parsetree.Pexp_newtype (x0,x1) ->
      Ast_407.Parsetree.Pexp_newtype
        ((copy_Ast_408_Asttypes_loc (fun x  -> x) x0),
          (copy_Ast_408_Parsetree_expression x1))
  | Ast_408.Parsetree.Pexp_pack x0 ->
      Ast_407.Parsetree.Pexp_pack (copy_Ast_408_Parsetree_module_expr x0)
  | Ast_408.Parsetree.Pexp_open (x0,x1) ->
    begin match x0.Ast_408.Parsetree.popen_expr.Ast_408.Parsetree.pmod_desc with
    | Pmod_ident lid ->
      Ast_407.Parsetree.Pexp_open
        (copy_Ast_408_Asttypes_override_flag x0.Ast_408.Parsetree.popen_override,
         (copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t lid),
         (copy_Ast_408_Parsetree_expression x1))
    | Pmod_structure _ | Pmod_functor _ | Pmod_apply _
    | Pmod_constraint _ | Pmod_unpack _ | Pmod_extension _ ->
      migration_error x0.Ast_408.Parsetree.popen_loc Def.Pexp_open
    end
  | Ast_408.Parsetree.Pexp_letop { let_; ands = _; body = _; } ->
    migration_error let_.pbop_op.loc Def.Pexp_letop
  | Ast_408.Parsetree.Pexp_extension x0 ->
      Ast_407.Parsetree.Pexp_extension (copy_Ast_408_Parsetree_extension x0)
  | Ast_408.Parsetree.Pexp_unreachable  -> Ast_407.Parsetree.Pexp_unreachable

and copy_Ast_408_Asttypes_direction_flag :
  Ast_408.Asttypes.direction_flag -> Ast_407.Asttypes.direction_flag =
  function
  | Ast_408.Asttypes.Upto  -> Ast_407.Asttypes.Upto
  | Ast_408.Asttypes.Downto  -> Ast_407.Asttypes.Downto

and copy_Ast_408_Parsetree_case :
  Ast_408.Parsetree.case -> Ast_407.Parsetree.case =
  fun
    { Ast_408.Parsetree.pc_lhs = pc_lhs;
      Ast_408.Parsetree.pc_guard = pc_guard;
      Ast_408.Parsetree.pc_rhs = pc_rhs }
     ->
    {
      Ast_407.Parsetree.pc_lhs = (copy_Ast_408_Parsetree_pattern pc_lhs);
      Ast_407.Parsetree.pc_guard =
        (copy_option copy_Ast_408_Parsetree_expression pc_guard);
      Ast_407.Parsetree.pc_rhs = (copy_Ast_408_Parsetree_expression pc_rhs)
    }

and copy_Ast_408_Parsetree_value_binding :
  Ast_408.Parsetree.value_binding -> Ast_407.Parsetree.value_binding =
  fun
    { Ast_408.Parsetree.pvb_pat = pvb_pat;
      Ast_408.Parsetree.pvb_expr = pvb_expr;
      Ast_408.Parsetree.pvb_attributes = pvb_attributes;
      Ast_408.Parsetree.pvb_loc = pvb_loc }
     ->
    {
      Ast_407.Parsetree.pvb_pat = (copy_Ast_408_Parsetree_pattern pvb_pat);
      Ast_407.Parsetree.pvb_expr =
        (copy_Ast_408_Parsetree_expression pvb_expr);
      Ast_407.Parsetree.pvb_attributes =
        (copy_Ast_408_Parsetree_attributes pvb_attributes);
      Ast_407.Parsetree.pvb_loc = (copy_Ast_408_Location_t pvb_loc)
    }

and copy_Ast_408_Parsetree_pattern :
  Ast_408.Parsetree.pattern -> Ast_407.Parsetree.pattern =
  fun
    { Ast_408.Parsetree.ppat_desc = ppat_desc;
      Ast_408.Parsetree.ppat_loc = ppat_loc;
      Ast_408.Parsetree.ppat_loc_stack = _;
      Ast_408.Parsetree.ppat_attributes = ppat_attributes }
     ->
    {
      Ast_407.Parsetree.ppat_desc =
        (copy_Ast_408_Parsetree_pattern_desc ppat_desc);
      Ast_407.Parsetree.ppat_loc = (copy_Ast_408_Location_t ppat_loc);
      Ast_407.Parsetree.ppat_attributes =
        (copy_Ast_408_Parsetree_attributes ppat_attributes)
    }

and copy_Ast_408_Parsetree_pattern_desc :
  Ast_408.Parsetree.pattern_desc -> Ast_407.Parsetree.pattern_desc =
  function
  | Ast_408.Parsetree.Ppat_any  -> Ast_407.Parsetree.Ppat_any
  | Ast_408.Parsetree.Ppat_var x0 ->
      Ast_407.Parsetree.Ppat_var (copy_Ast_408_Asttypes_loc (fun x  -> x) x0)
  | Ast_408.Parsetree.Ppat_alias (x0,x1) ->
      Ast_407.Parsetree.Ppat_alias
        ((copy_Ast_408_Parsetree_pattern x0),
          (copy_Ast_408_Asttypes_loc (fun x  -> x) x1))
  | Ast_408.Parsetree.Ppat_constant x0 ->
      Ast_407.Parsetree.Ppat_constant (copy_Ast_408_Parsetree_constant x0)
  | Ast_408.Parsetree.Ppat_interval (x0,x1) ->
      Ast_407.Parsetree.Ppat_interval
        ((copy_Ast_408_Parsetree_constant x0),
          (copy_Ast_408_Parsetree_constant x1))
  | Ast_408.Parsetree.Ppat_tuple x0 ->
      Ast_407.Parsetree.Ppat_tuple
        (List.map copy_Ast_408_Parsetree_pattern x0)
  | Ast_408.Parsetree.Ppat_construct (x0,x1) ->
      Ast_407.Parsetree.Ppat_construct
        ((copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0),
          (copy_option copy_Ast_408_Parsetree_pattern x1))
  | Ast_408.Parsetree.Ppat_variant (x0,x1) ->
      Ast_407.Parsetree.Ppat_variant
        ((copy_Ast_408_Asttypes_label x0),
          (copy_option copy_Ast_408_Parsetree_pattern x1))
  | Ast_408.Parsetree.Ppat_record (x0,x1) ->
      Ast_407.Parsetree.Ppat_record
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               ((copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0),
                 (copy_Ast_408_Parsetree_pattern x1))) x0),
          (copy_Ast_408_Asttypes_closed_flag x1))
  | Ast_408.Parsetree.Ppat_array x0 ->
      Ast_407.Parsetree.Ppat_array
        (List.map copy_Ast_408_Parsetree_pattern x0)
  | Ast_408.Parsetree.Ppat_or (x0,x1) ->
      Ast_407.Parsetree.Ppat_or
        ((copy_Ast_408_Parsetree_pattern x0),
          (copy_Ast_408_Parsetree_pattern x1))
  | Ast_408.Parsetree.Ppat_constraint (x0,x1) ->
      Ast_407.Parsetree.Ppat_constraint
        ((copy_Ast_408_Parsetree_pattern x0),
          (copy_Ast_408_Parsetree_core_type x1))
  | Ast_408.Parsetree.Ppat_type x0 ->
      Ast_407.Parsetree.Ppat_type
        (copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0)
  | Ast_408.Parsetree.Ppat_lazy x0 ->
      Ast_407.Parsetree.Ppat_lazy (copy_Ast_408_Parsetree_pattern x0)
  | Ast_408.Parsetree.Ppat_unpack x0 ->
      Ast_407.Parsetree.Ppat_unpack
        (copy_Ast_408_Asttypes_loc (fun x  -> x) x0)
  | Ast_408.Parsetree.Ppat_exception x0 ->
      Ast_407.Parsetree.Ppat_exception (copy_Ast_408_Parsetree_pattern x0)
  | Ast_408.Parsetree.Ppat_extension x0 ->
      Ast_407.Parsetree.Ppat_extension (copy_Ast_408_Parsetree_extension x0)
  | Ast_408.Parsetree.Ppat_open (x0,x1) ->
      Ast_407.Parsetree.Ppat_open
        ((copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0),
          (copy_Ast_408_Parsetree_pattern x1))

and copy_Ast_408_Parsetree_core_type :
  Ast_408.Parsetree.core_type -> Ast_407.Parsetree.core_type =
  fun
    { Ast_408.Parsetree.ptyp_desc = ptyp_desc;
      Ast_408.Parsetree.ptyp_loc = ptyp_loc;
      Ast_408.Parsetree.ptyp_loc_stack = _;
      Ast_408.Parsetree.ptyp_attributes = ptyp_attributes }
     ->
    {
      Ast_407.Parsetree.ptyp_desc =
        (copy_Ast_408_Parsetree_core_type_desc ptyp_desc);
      Ast_407.Parsetree.ptyp_loc = (copy_Ast_408_Location_t ptyp_loc);
      Ast_407.Parsetree.ptyp_attributes =
        (copy_Ast_408_Parsetree_attributes ptyp_attributes)
    }

and copy_Ast_408_Parsetree_core_type_desc :
  Ast_408.Parsetree.core_type_desc -> Ast_407.Parsetree.core_type_desc =
  function
  | Ast_408.Parsetree.Ptyp_any  -> Ast_407.Parsetree.Ptyp_any
  | Ast_408.Parsetree.Ptyp_var x0 -> Ast_407.Parsetree.Ptyp_var x0
  | Ast_408.Parsetree.Ptyp_arrow (x0,x1,x2) ->
      Ast_407.Parsetree.Ptyp_arrow
        ((copy_Ast_408_Asttypes_arg_label x0),
          (copy_Ast_408_Parsetree_core_type x1),
          (copy_Ast_408_Parsetree_core_type x2))
  | Ast_408.Parsetree.Ptyp_tuple x0 ->
      Ast_407.Parsetree.Ptyp_tuple
        (List.map copy_Ast_408_Parsetree_core_type x0)
  | Ast_408.Parsetree.Ptyp_constr (x0,x1) ->
      Ast_407.Parsetree.Ptyp_constr
        ((copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0),
          (List.map copy_Ast_408_Parsetree_core_type x1))
  | Ast_408.Parsetree.Ptyp_object (x0,x1) ->
      Ast_407.Parsetree.Ptyp_object
        ((List.map copy_Ast_408_Parsetree_object_field x0),
          (copy_Ast_408_Asttypes_closed_flag x1))
  | Ast_408.Parsetree.Ptyp_class (x0,x1) ->
      Ast_407.Parsetree.Ptyp_class
        ((copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0),
          (List.map copy_Ast_408_Parsetree_core_type x1))
  | Ast_408.Parsetree.Ptyp_alias (x0,x1) ->
      Ast_407.Parsetree.Ptyp_alias
        ((copy_Ast_408_Parsetree_core_type x0), x1)
  | Ast_408.Parsetree.Ptyp_variant (x0,x1,x2) ->
      Ast_407.Parsetree.Ptyp_variant
        ((List.map copy_Ast_408_Parsetree_row_field x0),
          (copy_Ast_408_Asttypes_closed_flag x1),
          (copy_option (fun x  -> List.map copy_Ast_408_Asttypes_label x) x2))
  | Ast_408.Parsetree.Ptyp_poly (x0,x1) ->
      Ast_407.Parsetree.Ptyp_poly
        ((List.map (fun x  -> copy_Ast_408_Asttypes_loc (fun x  -> x) x) x0),
          (copy_Ast_408_Parsetree_core_type x1))
  | Ast_408.Parsetree.Ptyp_package x0 ->
      Ast_407.Parsetree.Ptyp_package (copy_Ast_408_Parsetree_package_type x0)
  | Ast_408.Parsetree.Ptyp_extension x0 ->
      Ast_407.Parsetree.Ptyp_extension (copy_Ast_408_Parsetree_extension x0)

and copy_Ast_408_Parsetree_package_type :
  Ast_408.Parsetree.package_type -> Ast_407.Parsetree.package_type =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0),
      (List.map
         (fun x  ->
            let (x0,x1) = x  in
            ((copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0),
              (copy_Ast_408_Parsetree_core_type x1))) x1))

and copy_Ast_408_Parsetree_row_field :
  Ast_408.Parsetree.row_field -> Ast_407.Parsetree.row_field =
  fun
    { Ast_408.Parsetree.prf_desc = prf_desc;
      Ast_408.Parsetree.prf_loc = _;
      Ast_408.Parsetree.prf_attributes = prf_attributes }
    ->
      match prf_desc with
      | Ast_408.Parsetree.Rtag (x0, x1, x2) ->
        Ast_407.Parsetree.Rtag ((copy_Ast_408_Asttypes_loc copy_Ast_408_Asttypes_label x0),
                                (copy_Ast_408_Parsetree_attributes prf_attributes),
                                (copy_bool x1),
                                (List.map copy_Ast_408_Parsetree_core_type x2))
      | Ast_408.Parsetree.Rinherit x0 ->
        Ast_407.Parsetree.Rinherit (copy_Ast_408_Parsetree_core_type x0)

and copy_Ast_408_Parsetree_object_field :
  Ast_408.Parsetree.object_field -> Ast_407.Parsetree.object_field =
  fun
    { Ast_408.Parsetree.pof_desc = pof_desc;
      Ast_408.Parsetree.pof_loc = _;
      Ast_408.Parsetree.pof_attributes = pof_attributes }
    ->
      match pof_desc with
      | Ast_408.Parsetree.Otag (x0, x1) ->
        Ast_407.Parsetree.Otag ((copy_Ast_408_Asttypes_loc copy_Ast_408_Asttypes_label x0),
                                (copy_Ast_408_Parsetree_attributes pof_attributes),
                                (copy_Ast_408_Parsetree_core_type x1))
      | Ast_408.Parsetree.Oinherit x0 ->
        Ast_407.Parsetree.Oinherit (copy_Ast_408_Parsetree_core_type x0)

and copy_Ast_408_Parsetree_attributes :
  Ast_408.Parsetree.attributes -> Ast_407.Parsetree.attributes =
  fun x  -> List.map copy_Ast_408_Parsetree_attribute x

and copy_Ast_408_Parsetree_attribute :
  Ast_408.Parsetree.attribute -> Ast_407.Parsetree.attribute =
  fun
    { Ast_408.Parsetree.attr_name = attr_name;
      Ast_408.Parsetree.attr_payload = attr_payload;
      Ast_408.Parsetree.attr_loc = _ }
    ->
      ((copy_Ast_408_Asttypes_loc (fun x  -> x) attr_name),
       (copy_Ast_408_Parsetree_payload attr_payload))

and copy_Ast_408_Parsetree_payload :
  Ast_408.Parsetree.payload -> Ast_407.Parsetree.payload =
  function
  | Ast_408.Parsetree.PStr x0 ->
      Ast_407.Parsetree.PStr (copy_Ast_408_Parsetree_structure x0)
  | Ast_408.Parsetree.PSig x0 ->
      Ast_407.Parsetree.PSig (copy_Ast_408_Parsetree_signature x0)
  | Ast_408.Parsetree.PTyp x0 ->
      Ast_407.Parsetree.PTyp (copy_Ast_408_Parsetree_core_type x0)
  | Ast_408.Parsetree.PPat (x0,x1) ->
      Ast_407.Parsetree.PPat
        ((copy_Ast_408_Parsetree_pattern x0),
          (copy_option copy_Ast_408_Parsetree_expression x1))

and copy_Ast_408_Parsetree_structure :
  Ast_408.Parsetree.structure -> Ast_407.Parsetree.structure =
  fun x  -> List.map copy_Ast_408_Parsetree_structure_item x

and copy_Ast_408_Parsetree_structure_item :
  Ast_408.Parsetree.structure_item -> Ast_407.Parsetree.structure_item =
  fun
    { Ast_408.Parsetree.pstr_desc = pstr_desc;
      Ast_408.Parsetree.pstr_loc = pstr_loc }
     ->
    {
      Ast_407.Parsetree.pstr_desc =
        (copy_Ast_408_Parsetree_structure_item_desc pstr_desc);
      Ast_407.Parsetree.pstr_loc = (copy_Ast_408_Location_t pstr_loc)
    }

and copy_Ast_408_Parsetree_structure_item_desc :
  Ast_408.Parsetree.structure_item_desc ->
    Ast_407.Parsetree.structure_item_desc
  =
  function
  | Ast_408.Parsetree.Pstr_eval (x0,x1) ->
      Ast_407.Parsetree.Pstr_eval
        ((copy_Ast_408_Parsetree_expression x0),
          (copy_Ast_408_Parsetree_attributes x1))
  | Ast_408.Parsetree.Pstr_value (x0,x1) ->
      Ast_407.Parsetree.Pstr_value
        ((copy_Ast_408_Asttypes_rec_flag x0),
          (List.map copy_Ast_408_Parsetree_value_binding x1))
  | Ast_408.Parsetree.Pstr_primitive x0 ->
      Ast_407.Parsetree.Pstr_primitive
        (copy_Ast_408_Parsetree_value_description x0)
  | Ast_408.Parsetree.Pstr_type (x0,x1) ->
      Ast_407.Parsetree.Pstr_type
        ((copy_Ast_408_Asttypes_rec_flag x0),
          (List.map copy_Ast_408_Parsetree_type_declaration x1))
  | Ast_408.Parsetree.Pstr_typext x0 ->
      Ast_407.Parsetree.Pstr_typext
        (copy_Ast_408_Parsetree_type_extension x0)
  | Ast_408.Parsetree.Pstr_exception x0 ->
      Ast_407.Parsetree.Pstr_exception
        (copy_Ast_408_Parsetree_extension_constructor
           x0.Ast_408.Parsetree.ptyexn_constructor)
  | Ast_408.Parsetree.Pstr_module x0 ->
      Ast_407.Parsetree.Pstr_module
        (copy_Ast_408_Parsetree_module_binding x0)
  | Ast_408.Parsetree.Pstr_recmodule x0 ->
      Ast_407.Parsetree.Pstr_recmodule
        (List.map copy_Ast_408_Parsetree_module_binding x0)
  | Ast_408.Parsetree.Pstr_modtype x0 ->
      Ast_407.Parsetree.Pstr_modtype
        (copy_Ast_408_Parsetree_module_type_declaration x0)
  | Ast_408.Parsetree.Pstr_open x0 ->
    begin match x0.Ast_408.Parsetree.popen_expr.Ast_408.Parsetree.pmod_desc with
    | Pmod_ident lid ->
      Ast_407.Parsetree.Pstr_open
        { Ast_407.Parsetree.popen_lid = (copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t lid);
          Ast_407.Parsetree.popen_override = (copy_Ast_408_Asttypes_override_flag x0.Ast_408.Parsetree.popen_override);
          Ast_407.Parsetree.popen_loc = (copy_Ast_408_Location_t x0.Ast_408.Parsetree.popen_loc);
          Ast_407.Parsetree.popen_attributes = (copy_Ast_408_Parsetree_attributes x0.Ast_408.Parsetree.popen_attributes); }
    | Pmod_structure _ | Pmod_functor _ | Pmod_apply _
    | Pmod_constraint _ | Pmod_unpack _ | Pmod_extension _ ->
      migration_error x0.Ast_408.Parsetree.popen_loc Def.Pexp_open
    end
  | Ast_408.Parsetree.Pstr_class x0 ->
      Ast_407.Parsetree.Pstr_class
        (List.map copy_Ast_408_Parsetree_class_declaration x0)
  | Ast_408.Parsetree.Pstr_class_type x0 ->
      Ast_407.Parsetree.Pstr_class_type
        (List.map copy_Ast_408_Parsetree_class_type_declaration x0)
  | Ast_408.Parsetree.Pstr_include x0 ->
      Ast_407.Parsetree.Pstr_include
        (copy_Ast_408_Parsetree_include_declaration x0)
  | Ast_408.Parsetree.Pstr_attribute x0 ->
      Ast_407.Parsetree.Pstr_attribute (copy_Ast_408_Parsetree_attribute x0)
  | Ast_408.Parsetree.Pstr_extension (x0,x1) ->
      Ast_407.Parsetree.Pstr_extension
        ((copy_Ast_408_Parsetree_extension x0),
          (copy_Ast_408_Parsetree_attributes x1))

and copy_Ast_408_Parsetree_include_declaration :
  Ast_408.Parsetree.include_declaration ->
    Ast_407.Parsetree.include_declaration
  =
  fun x  ->
    copy_Ast_408_Parsetree_include_infos copy_Ast_408_Parsetree_module_expr x

and copy_Ast_408_Parsetree_class_declaration :
  Ast_408.Parsetree.class_declaration -> Ast_407.Parsetree.class_declaration
  =
  fun x  ->
    copy_Ast_408_Parsetree_class_infos copy_Ast_408_Parsetree_class_expr x

and copy_Ast_408_Parsetree_class_expr :
  Ast_408.Parsetree.class_expr -> Ast_407.Parsetree.class_expr =
  fun
    { Ast_408.Parsetree.pcl_desc = pcl_desc;
      Ast_408.Parsetree.pcl_loc = pcl_loc;
      Ast_408.Parsetree.pcl_attributes = pcl_attributes }
     ->
    {
      Ast_407.Parsetree.pcl_desc =
        (copy_Ast_408_Parsetree_class_expr_desc pcl_desc);
      Ast_407.Parsetree.pcl_loc = (copy_Ast_408_Location_t pcl_loc);
      Ast_407.Parsetree.pcl_attributes =
        (copy_Ast_408_Parsetree_attributes pcl_attributes)
    }

and copy_Ast_408_Parsetree_class_expr_desc :
  Ast_408.Parsetree.class_expr_desc -> Ast_407.Parsetree.class_expr_desc =
  function
  | Ast_408.Parsetree.Pcl_constr (x0,x1) ->
      Ast_407.Parsetree.Pcl_constr
        ((copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0),
          (List.map copy_Ast_408_Parsetree_core_type x1))
  | Ast_408.Parsetree.Pcl_structure x0 ->
      Ast_407.Parsetree.Pcl_structure
        (copy_Ast_408_Parsetree_class_structure x0)
  | Ast_408.Parsetree.Pcl_fun (x0,x1,x2,x3) ->
      Ast_407.Parsetree.Pcl_fun
        ((copy_Ast_408_Asttypes_arg_label x0),
          (copy_option copy_Ast_408_Parsetree_expression x1),
          (copy_Ast_408_Parsetree_pattern x2),
          (copy_Ast_408_Parsetree_class_expr x3))
  | Ast_408.Parsetree.Pcl_apply (x0,x1) ->
      Ast_407.Parsetree.Pcl_apply
        ((copy_Ast_408_Parsetree_class_expr x0),
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_Ast_408_Asttypes_arg_label x0),
                  (copy_Ast_408_Parsetree_expression x1))) x1))
  | Ast_408.Parsetree.Pcl_let (x0,x1,x2) ->
      Ast_407.Parsetree.Pcl_let
        ((copy_Ast_408_Asttypes_rec_flag x0),
          (List.map copy_Ast_408_Parsetree_value_binding x1),
          (copy_Ast_408_Parsetree_class_expr x2))
  | Ast_408.Parsetree.Pcl_constraint (x0,x1) ->
      Ast_407.Parsetree.Pcl_constraint
        ((copy_Ast_408_Parsetree_class_expr x0),
          (copy_Ast_408_Parsetree_class_type x1))
  | Ast_408.Parsetree.Pcl_extension x0 ->
      Ast_407.Parsetree.Pcl_extension (copy_Ast_408_Parsetree_extension x0)
  | Ast_408.Parsetree.Pcl_open (x0,x1) ->
    Ast_407.Parsetree.Pcl_open
      ((copy_Ast_408_Asttypes_override_flag x0.Ast_408.Parsetree.popen_override),
       (copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0.Ast_408.Parsetree.popen_expr),
       (copy_Ast_408_Parsetree_class_expr x1))

and copy_Ast_408_Parsetree_class_structure :
  Ast_408.Parsetree.class_structure -> Ast_407.Parsetree.class_structure =
  fun
    { Ast_408.Parsetree.pcstr_self = pcstr_self;
      Ast_408.Parsetree.pcstr_fields = pcstr_fields }
     ->
    {
      Ast_407.Parsetree.pcstr_self =
        (copy_Ast_408_Parsetree_pattern pcstr_self);
      Ast_407.Parsetree.pcstr_fields =
        (List.map copy_Ast_408_Parsetree_class_field pcstr_fields)
    }

and copy_Ast_408_Parsetree_class_field :
  Ast_408.Parsetree.class_field -> Ast_407.Parsetree.class_field =
  fun
    { Ast_408.Parsetree.pcf_desc = pcf_desc;
      Ast_408.Parsetree.pcf_loc = pcf_loc;
      Ast_408.Parsetree.pcf_attributes = pcf_attributes }
     ->
    {
      Ast_407.Parsetree.pcf_desc =
        (copy_Ast_408_Parsetree_class_field_desc pcf_desc);
      Ast_407.Parsetree.pcf_loc = (copy_Ast_408_Location_t pcf_loc);
      Ast_407.Parsetree.pcf_attributes =
        (copy_Ast_408_Parsetree_attributes pcf_attributes)
    }

and copy_Ast_408_Parsetree_class_field_desc :
  Ast_408.Parsetree.class_field_desc -> Ast_407.Parsetree.class_field_desc =
  function
  | Ast_408.Parsetree.Pcf_inherit (x0,x1,x2) ->
      Ast_407.Parsetree.Pcf_inherit
        ((copy_Ast_408_Asttypes_override_flag x0),
          (copy_Ast_408_Parsetree_class_expr x1),
          (copy_option (fun x  -> copy_Ast_408_Asttypes_loc (fun x  -> x) x)
             x2))
  | Ast_408.Parsetree.Pcf_val x0 ->
      Ast_407.Parsetree.Pcf_val
        (let (x0,x1,x2) = x0  in
         ((copy_Ast_408_Asttypes_loc copy_Ast_408_Asttypes_label x0),
           (copy_Ast_408_Asttypes_mutable_flag x1),
           (copy_Ast_408_Parsetree_class_field_kind x2)))
  | Ast_408.Parsetree.Pcf_method x0 ->
      Ast_407.Parsetree.Pcf_method
        (let (x0,x1,x2) = x0  in
         ((copy_Ast_408_Asttypes_loc copy_Ast_408_Asttypes_label x0),
           (copy_Ast_408_Asttypes_private_flag x1),
           (copy_Ast_408_Parsetree_class_field_kind x2)))
  | Ast_408.Parsetree.Pcf_constraint x0 ->
      Ast_407.Parsetree.Pcf_constraint
        (let (x0,x1) = x0  in
         ((copy_Ast_408_Parsetree_core_type x0),
           (copy_Ast_408_Parsetree_core_type x1)))
  | Ast_408.Parsetree.Pcf_initializer x0 ->
      Ast_407.Parsetree.Pcf_initializer
        (copy_Ast_408_Parsetree_expression x0)
  | Ast_408.Parsetree.Pcf_attribute x0 ->
      Ast_407.Parsetree.Pcf_attribute (copy_Ast_408_Parsetree_attribute x0)
  | Ast_408.Parsetree.Pcf_extension x0 ->
      Ast_407.Parsetree.Pcf_extension (copy_Ast_408_Parsetree_extension x0)

and copy_Ast_408_Parsetree_class_field_kind :
  Ast_408.Parsetree.class_field_kind -> Ast_407.Parsetree.class_field_kind =
  function
  | Ast_408.Parsetree.Cfk_virtual x0 ->
      Ast_407.Parsetree.Cfk_virtual (copy_Ast_408_Parsetree_core_type x0)
  | Ast_408.Parsetree.Cfk_concrete (x0,x1) ->
      Ast_407.Parsetree.Cfk_concrete
        ((copy_Ast_408_Asttypes_override_flag x0),
          (copy_Ast_408_Parsetree_expression x1))

and copy_Ast_408_Parsetree_module_binding :
  Ast_408.Parsetree.module_binding -> Ast_407.Parsetree.module_binding =
  fun
    { Ast_408.Parsetree.pmb_name = pmb_name;
      Ast_408.Parsetree.pmb_expr = pmb_expr;
      Ast_408.Parsetree.pmb_attributes = pmb_attributes;
      Ast_408.Parsetree.pmb_loc = pmb_loc }
     ->
    {
      Ast_407.Parsetree.pmb_name =
        (copy_Ast_408_Asttypes_loc (fun x  -> x) pmb_name);
      Ast_407.Parsetree.pmb_expr =
        (copy_Ast_408_Parsetree_module_expr pmb_expr);
      Ast_407.Parsetree.pmb_attributes =
        (copy_Ast_408_Parsetree_attributes pmb_attributes);
      Ast_407.Parsetree.pmb_loc = (copy_Ast_408_Location_t pmb_loc)
    }

and copy_Ast_408_Parsetree_module_expr :
  Ast_408.Parsetree.module_expr -> Ast_407.Parsetree.module_expr =
  fun
    { Ast_408.Parsetree.pmod_desc = pmod_desc;
      Ast_408.Parsetree.pmod_loc = pmod_loc;
      Ast_408.Parsetree.pmod_attributes = pmod_attributes }
     ->
    {
      Ast_407.Parsetree.pmod_desc =
        (copy_Ast_408_Parsetree_module_expr_desc pmod_desc);
      Ast_407.Parsetree.pmod_loc = (copy_Ast_408_Location_t pmod_loc);
      Ast_407.Parsetree.pmod_attributes =
        (copy_Ast_408_Parsetree_attributes pmod_attributes)
    }

and copy_Ast_408_Parsetree_module_expr_desc :
  Ast_408.Parsetree.module_expr_desc -> Ast_407.Parsetree.module_expr_desc =
  function
  | Ast_408.Parsetree.Pmod_ident x0 ->
      Ast_407.Parsetree.Pmod_ident
        (copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0)
  | Ast_408.Parsetree.Pmod_structure x0 ->
      Ast_407.Parsetree.Pmod_structure (copy_Ast_408_Parsetree_structure x0)
  | Ast_408.Parsetree.Pmod_functor (x0,x1,x2) ->
      Ast_407.Parsetree.Pmod_functor
        ((copy_Ast_408_Asttypes_loc (fun x  -> x) x0),
          (copy_option copy_Ast_408_Parsetree_module_type x1),
          (copy_Ast_408_Parsetree_module_expr x2))
  | Ast_408.Parsetree.Pmod_apply (x0,x1) ->
      Ast_407.Parsetree.Pmod_apply
        ((copy_Ast_408_Parsetree_module_expr x0),
          (copy_Ast_408_Parsetree_module_expr x1))
  | Ast_408.Parsetree.Pmod_constraint (x0,x1) ->
      Ast_407.Parsetree.Pmod_constraint
        ((copy_Ast_408_Parsetree_module_expr x0),
          (copy_Ast_408_Parsetree_module_type x1))
  | Ast_408.Parsetree.Pmod_unpack x0 ->
      Ast_407.Parsetree.Pmod_unpack (copy_Ast_408_Parsetree_expression x0)
  | Ast_408.Parsetree.Pmod_extension x0 ->
      Ast_407.Parsetree.Pmod_extension (copy_Ast_408_Parsetree_extension x0)

and copy_Ast_408_Parsetree_module_type :
  Ast_408.Parsetree.module_type -> Ast_407.Parsetree.module_type =
  fun
    { Ast_408.Parsetree.pmty_desc = pmty_desc;
      Ast_408.Parsetree.pmty_loc = pmty_loc;
      Ast_408.Parsetree.pmty_attributes = pmty_attributes }
     ->
    {
      Ast_407.Parsetree.pmty_desc =
        (copy_Ast_408_Parsetree_module_type_desc pmty_desc);
      Ast_407.Parsetree.pmty_loc = (copy_Ast_408_Location_t pmty_loc);
      Ast_407.Parsetree.pmty_attributes =
        (copy_Ast_408_Parsetree_attributes pmty_attributes)
    }

and copy_Ast_408_Parsetree_module_type_desc :
  Ast_408.Parsetree.module_type_desc -> Ast_407.Parsetree.module_type_desc =
  function
  | Ast_408.Parsetree.Pmty_ident x0 ->
      Ast_407.Parsetree.Pmty_ident
        (copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0)
  | Ast_408.Parsetree.Pmty_signature x0 ->
      Ast_407.Parsetree.Pmty_signature (copy_Ast_408_Parsetree_signature x0)
  | Ast_408.Parsetree.Pmty_functor (x0,x1,x2) ->
      Ast_407.Parsetree.Pmty_functor
        ((copy_Ast_408_Asttypes_loc (fun x  -> x) x0),
          (copy_option copy_Ast_408_Parsetree_module_type x1),
          (copy_Ast_408_Parsetree_module_type x2))
  | Ast_408.Parsetree.Pmty_with (x0,x1) ->
      Ast_407.Parsetree.Pmty_with
        ((copy_Ast_408_Parsetree_module_type x0),
          (List.map copy_Ast_408_Parsetree_with_constraint x1))
  | Ast_408.Parsetree.Pmty_typeof x0 ->
      Ast_407.Parsetree.Pmty_typeof (copy_Ast_408_Parsetree_module_expr x0)
  | Ast_408.Parsetree.Pmty_extension x0 ->
      Ast_407.Parsetree.Pmty_extension (copy_Ast_408_Parsetree_extension x0)
  | Ast_408.Parsetree.Pmty_alias x0 ->
      Ast_407.Parsetree.Pmty_alias
        (copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0)

and copy_Ast_408_Parsetree_with_constraint :
  Ast_408.Parsetree.with_constraint -> Ast_407.Parsetree.with_constraint =
  function
  | Ast_408.Parsetree.Pwith_type (x0,x1) ->
      Ast_407.Parsetree.Pwith_type
        ((copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0),
          (copy_Ast_408_Parsetree_type_declaration x1))
  | Ast_408.Parsetree.Pwith_module (x0,x1) ->
      Ast_407.Parsetree.Pwith_module
        ((copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0),
          (copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x1))
  | Ast_408.Parsetree.Pwith_typesubst (x0,x1) ->
      Ast_407.Parsetree.Pwith_typesubst
        ((copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0),
          (copy_Ast_408_Parsetree_type_declaration x1))
  | Ast_408.Parsetree.Pwith_modsubst (x0,x1) ->
      Ast_407.Parsetree.Pwith_modsubst
        ((copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0),
          (copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x1))

and copy_Ast_408_Parsetree_signature :
  Ast_408.Parsetree.signature -> Ast_407.Parsetree.signature =
  fun x  -> List.map copy_Ast_408_Parsetree_signature_item x

and copy_Ast_408_Parsetree_signature_item :
  Ast_408.Parsetree.signature_item -> Ast_407.Parsetree.signature_item =
  fun
    { Ast_408.Parsetree.psig_desc = psig_desc;
      Ast_408.Parsetree.psig_loc = psig_loc }
     ->
    {
      Ast_407.Parsetree.psig_desc =
        (copy_Ast_408_Parsetree_signature_item_desc psig_desc);
      Ast_407.Parsetree.psig_loc = (copy_Ast_408_Location_t psig_loc)
    }

and copy_Ast_408_Parsetree_signature_item_desc :
  Ast_408.Parsetree.signature_item_desc ->
    Ast_407.Parsetree.signature_item_desc
  =
  function
  | Ast_408.Parsetree.Psig_value x0 ->
      Ast_407.Parsetree.Psig_value
        (copy_Ast_408_Parsetree_value_description x0)
  | Ast_408.Parsetree.Psig_type (x0,x1) ->
      Ast_407.Parsetree.Psig_type
        ((copy_Ast_408_Asttypes_rec_flag x0),
          (List.map copy_Ast_408_Parsetree_type_declaration x1))
  | Ast_408.Parsetree.Psig_typesubst x0 ->
    let x0_loc =
      match x0 with
      | [] -> Location.none
      | { Ast_408.Parsetree.ptype_loc; _ } :: _ -> ptype_loc in
    migration_error x0_loc Def.Psig_typesubst
  | Ast_408.Parsetree.Psig_typext x0 ->
      Ast_407.Parsetree.Psig_typext
        (copy_Ast_408_Parsetree_type_extension x0)
  | Ast_408.Parsetree.Psig_exception x0 ->
      Ast_407.Parsetree.Psig_exception
        (copy_Ast_408_Parsetree_extension_constructor
           x0.Ast_408.Parsetree.ptyexn_constructor)
  | Ast_408.Parsetree.Psig_module x0 ->
      Ast_407.Parsetree.Psig_module
        (copy_Ast_408_Parsetree_module_declaration x0)
  | Ast_408.Parsetree.Psig_modsubst x0 ->
      migration_error x0.pms_loc Def.Psig_modsubst
  | Ast_408.Parsetree.Psig_recmodule x0 ->
      Ast_407.Parsetree.Psig_recmodule
        (List.map copy_Ast_408_Parsetree_module_declaration x0)
  | Ast_408.Parsetree.Psig_modtype x0 ->
      Ast_407.Parsetree.Psig_modtype
        (copy_Ast_408_Parsetree_module_type_declaration x0)
  | Ast_408.Parsetree.Psig_open x0 ->
      Ast_407.Parsetree.Psig_open
        (copy_Ast_408_Parsetree_open_description x0)
  | Ast_408.Parsetree.Psig_include x0 ->
      Ast_407.Parsetree.Psig_include
        (copy_Ast_408_Parsetree_include_description x0)
  | Ast_408.Parsetree.Psig_class x0 ->
      Ast_407.Parsetree.Psig_class
        (List.map copy_Ast_408_Parsetree_class_description x0)
  | Ast_408.Parsetree.Psig_class_type x0 ->
      Ast_407.Parsetree.Psig_class_type
        (List.map copy_Ast_408_Parsetree_class_type_declaration x0)
  | Ast_408.Parsetree.Psig_attribute x0 ->
      Ast_407.Parsetree.Psig_attribute (copy_Ast_408_Parsetree_attribute x0)
  | Ast_408.Parsetree.Psig_extension (x0,x1) ->
      Ast_407.Parsetree.Psig_extension
        ((copy_Ast_408_Parsetree_extension x0),
          (copy_Ast_408_Parsetree_attributes x1))

and copy_Ast_408_Parsetree_class_type_declaration :
  Ast_408.Parsetree.class_type_declaration ->
    Ast_407.Parsetree.class_type_declaration
  =
  fun x  ->
    copy_Ast_408_Parsetree_class_infos copy_Ast_408_Parsetree_class_type x

and copy_Ast_408_Parsetree_class_description :
  Ast_408.Parsetree.class_description -> Ast_407.Parsetree.class_description
  =
  fun x  ->
    copy_Ast_408_Parsetree_class_infos copy_Ast_408_Parsetree_class_type x

and copy_Ast_408_Parsetree_class_type :
  Ast_408.Parsetree.class_type -> Ast_407.Parsetree.class_type =
  fun
    { Ast_408.Parsetree.pcty_desc = pcty_desc;
      Ast_408.Parsetree.pcty_loc = pcty_loc;
      Ast_408.Parsetree.pcty_attributes = pcty_attributes }
     ->
    {
      Ast_407.Parsetree.pcty_desc =
        (copy_Ast_408_Parsetree_class_type_desc pcty_desc);
      Ast_407.Parsetree.pcty_loc = (copy_Ast_408_Location_t pcty_loc);
      Ast_407.Parsetree.pcty_attributes =
        (copy_Ast_408_Parsetree_attributes pcty_attributes)
    }

and copy_Ast_408_Parsetree_class_type_desc :
  Ast_408.Parsetree.class_type_desc -> Ast_407.Parsetree.class_type_desc =
  function
  | Ast_408.Parsetree.Pcty_constr (x0,x1) ->
      Ast_407.Parsetree.Pcty_constr
        ((copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0),
          (List.map copy_Ast_408_Parsetree_core_type x1))
  | Ast_408.Parsetree.Pcty_signature x0 ->
      Ast_407.Parsetree.Pcty_signature
        (copy_Ast_408_Parsetree_class_signature x0)
  | Ast_408.Parsetree.Pcty_arrow (x0,x1,x2) ->
      Ast_407.Parsetree.Pcty_arrow
        ((copy_Ast_408_Asttypes_arg_label x0),
          (copy_Ast_408_Parsetree_core_type x1),
          (copy_Ast_408_Parsetree_class_type x2))
  | Ast_408.Parsetree.Pcty_extension x0 ->
      Ast_407.Parsetree.Pcty_extension (copy_Ast_408_Parsetree_extension x0)
  | Ast_408.Parsetree.Pcty_open (x0,x1) ->
    Ast_407.Parsetree.Pcty_open
      ((copy_Ast_408_Asttypes_override_flag x0.Ast_408.Parsetree.popen_override),
       (copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0.Ast_408.Parsetree.popen_expr),
       (copy_Ast_408_Parsetree_class_type x1))

and copy_Ast_408_Parsetree_class_signature :
  Ast_408.Parsetree.class_signature -> Ast_407.Parsetree.class_signature =
  fun
    { Ast_408.Parsetree.pcsig_self = pcsig_self;
      Ast_408.Parsetree.pcsig_fields = pcsig_fields }
     ->
    {
      Ast_407.Parsetree.pcsig_self =
        (copy_Ast_408_Parsetree_core_type pcsig_self);
      Ast_407.Parsetree.pcsig_fields =
        (List.map copy_Ast_408_Parsetree_class_type_field pcsig_fields)
    }

and copy_Ast_408_Parsetree_class_type_field :
  Ast_408.Parsetree.class_type_field -> Ast_407.Parsetree.class_type_field =
  fun
    { Ast_408.Parsetree.pctf_desc = pctf_desc;
      Ast_408.Parsetree.pctf_loc = pctf_loc;
      Ast_408.Parsetree.pctf_attributes = pctf_attributes }
     ->
    {
      Ast_407.Parsetree.pctf_desc =
        (copy_Ast_408_Parsetree_class_type_field_desc pctf_desc);
      Ast_407.Parsetree.pctf_loc = (copy_Ast_408_Location_t pctf_loc);
      Ast_407.Parsetree.pctf_attributes =
        (copy_Ast_408_Parsetree_attributes pctf_attributes)
    }

and copy_Ast_408_Parsetree_class_type_field_desc :
  Ast_408.Parsetree.class_type_field_desc ->
    Ast_407.Parsetree.class_type_field_desc
  =
  function
  | Ast_408.Parsetree.Pctf_inherit x0 ->
      Ast_407.Parsetree.Pctf_inherit (copy_Ast_408_Parsetree_class_type x0)
  | Ast_408.Parsetree.Pctf_val x0 ->
      Ast_407.Parsetree.Pctf_val
        (let (x0,x1,x2,x3) = x0  in
         ((copy_Ast_408_Asttypes_loc copy_Ast_408_Asttypes_label x0),
           (copy_Ast_408_Asttypes_mutable_flag x1),
           (copy_Ast_408_Asttypes_virtual_flag x2),
           (copy_Ast_408_Parsetree_core_type x3)))
  | Ast_408.Parsetree.Pctf_method x0 ->
      Ast_407.Parsetree.Pctf_method
        (let (x0,x1,x2,x3) = x0  in
         ((copy_Ast_408_Asttypes_loc copy_Ast_408_Asttypes_label x0),
           (copy_Ast_408_Asttypes_private_flag x1),
           (copy_Ast_408_Asttypes_virtual_flag x2),
           (copy_Ast_408_Parsetree_core_type x3)))
  | Ast_408.Parsetree.Pctf_constraint x0 ->
      Ast_407.Parsetree.Pctf_constraint
        (let (x0,x1) = x0  in
         ((copy_Ast_408_Parsetree_core_type x0),
           (copy_Ast_408_Parsetree_core_type x1)))
  | Ast_408.Parsetree.Pctf_attribute x0 ->
      Ast_407.Parsetree.Pctf_attribute (copy_Ast_408_Parsetree_attribute x0)
  | Ast_408.Parsetree.Pctf_extension x0 ->
      Ast_407.Parsetree.Pctf_extension (copy_Ast_408_Parsetree_extension x0)

and copy_Ast_408_Parsetree_extension :
  Ast_408.Parsetree.extension -> Ast_407.Parsetree.extension =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_Ast_408_Asttypes_loc (fun x  -> x) x0),
      (copy_Ast_408_Parsetree_payload x1))

and copy_Ast_408_Parsetree_class_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 Ast_408.Parsetree.class_infos -> 'g0 Ast_407.Parsetree.class_infos
  =
  fun f0  ->
    fun
      { Ast_408.Parsetree.pci_virt = pci_virt;
        Ast_408.Parsetree.pci_params = pci_params;
        Ast_408.Parsetree.pci_name = pci_name;
        Ast_408.Parsetree.pci_expr = pci_expr;
        Ast_408.Parsetree.pci_loc = pci_loc;
        Ast_408.Parsetree.pci_attributes = pci_attributes }
       ->
      {
        Ast_407.Parsetree.pci_virt =
          (copy_Ast_408_Asttypes_virtual_flag pci_virt);
        Ast_407.Parsetree.pci_params =
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_Ast_408_Parsetree_core_type x0),
                  (copy_Ast_408_Asttypes_variance x1))) pci_params);
        Ast_407.Parsetree.pci_name =
          (copy_Ast_408_Asttypes_loc (fun x  -> x) pci_name);
        Ast_407.Parsetree.pci_expr = (f0 pci_expr);
        Ast_407.Parsetree.pci_loc = (copy_Ast_408_Location_t pci_loc);
        Ast_407.Parsetree.pci_attributes =
          (copy_Ast_408_Parsetree_attributes pci_attributes)
      }

and copy_Ast_408_Asttypes_virtual_flag :
  Ast_408.Asttypes.virtual_flag -> Ast_407.Asttypes.virtual_flag =
  function
  | Ast_408.Asttypes.Virtual  -> Ast_407.Asttypes.Virtual
  | Ast_408.Asttypes.Concrete  -> Ast_407.Asttypes.Concrete

and copy_Ast_408_Parsetree_include_description :
  Ast_408.Parsetree.include_description ->
    Ast_407.Parsetree.include_description
  =
  fun x  ->
    copy_Ast_408_Parsetree_include_infos copy_Ast_408_Parsetree_module_type x

and copy_Ast_408_Parsetree_include_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 Ast_408.Parsetree.include_infos ->
        'g0 Ast_407.Parsetree.include_infos
  =
  fun f0  ->
    fun
      { Ast_408.Parsetree.pincl_mod = pincl_mod;
        Ast_408.Parsetree.pincl_loc = pincl_loc;
        Ast_408.Parsetree.pincl_attributes = pincl_attributes }
       ->
      {
        Ast_407.Parsetree.pincl_mod = (f0 pincl_mod);
        Ast_407.Parsetree.pincl_loc = (copy_Ast_408_Location_t pincl_loc);
        Ast_407.Parsetree.pincl_attributes =
          (copy_Ast_408_Parsetree_attributes pincl_attributes)
      }

and copy_Ast_408_Parsetree_open_description :
  Ast_408.Parsetree.open_description -> Ast_407.Parsetree.open_description =
  fun
    { Ast_408.Parsetree.popen_expr = popen_expr;
      Ast_408.Parsetree.popen_override = popen_override;
      Ast_408.Parsetree.popen_loc = popen_loc;
      Ast_408.Parsetree.popen_attributes = popen_attributes }
    ->
      { Ast_407.Parsetree.popen_lid = (copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t popen_expr);
        Ast_407.Parsetree.popen_override = (copy_Ast_408_Asttypes_override_flag popen_override);
        Ast_407.Parsetree.popen_loc = (copy_Ast_408_Location_t popen_loc);
        Ast_407.Parsetree.popen_attributes = (copy_Ast_408_Parsetree_attributes popen_attributes); }

and copy_Ast_408_Asttypes_override_flag :
  Ast_408.Asttypes.override_flag -> Ast_407.Asttypes.override_flag =
  function
  | Ast_408.Asttypes.Override  -> Ast_407.Asttypes.Override
  | Ast_408.Asttypes.Fresh  -> Ast_407.Asttypes.Fresh

and copy_Ast_408_Parsetree_module_type_declaration :
  Ast_408.Parsetree.module_type_declaration ->
    Ast_407.Parsetree.module_type_declaration
  =
  fun
    { Ast_408.Parsetree.pmtd_name = pmtd_name;
      Ast_408.Parsetree.pmtd_type = pmtd_type;
      Ast_408.Parsetree.pmtd_attributes = pmtd_attributes;
      Ast_408.Parsetree.pmtd_loc = pmtd_loc }
     ->
    {
      Ast_407.Parsetree.pmtd_name =
        (copy_Ast_408_Asttypes_loc (fun x  -> x) pmtd_name);
      Ast_407.Parsetree.pmtd_type =
        (copy_option copy_Ast_408_Parsetree_module_type pmtd_type);
      Ast_407.Parsetree.pmtd_attributes =
        (copy_Ast_408_Parsetree_attributes pmtd_attributes);
      Ast_407.Parsetree.pmtd_loc = (copy_Ast_408_Location_t pmtd_loc)
    }

and copy_Ast_408_Parsetree_module_declaration :
  Ast_408.Parsetree.module_declaration ->
    Ast_407.Parsetree.module_declaration
  =
  fun
    { Ast_408.Parsetree.pmd_name = pmd_name;
      Ast_408.Parsetree.pmd_type = pmd_type;
      Ast_408.Parsetree.pmd_attributes = pmd_attributes;
      Ast_408.Parsetree.pmd_loc = pmd_loc }
     ->
    {
      Ast_407.Parsetree.pmd_name =
        (copy_Ast_408_Asttypes_loc (fun x  -> x) pmd_name);
      Ast_407.Parsetree.pmd_type =
        (copy_Ast_408_Parsetree_module_type pmd_type);
      Ast_407.Parsetree.pmd_attributes =
        (copy_Ast_408_Parsetree_attributes pmd_attributes);
      Ast_407.Parsetree.pmd_loc = (copy_Ast_408_Location_t pmd_loc)
    }

(* and copy_Ast_408_Parsetree_type_exception :
  Ast_408.Parsetree.type_exception -> Ast_407.Parsetree.type_exception =
  fun
    { Ast_408.Parsetree.ptyexn_constructor = ptyexn_constructor;
      Ast_408.Parsetree.ptyexn_loc = ptyexn_loc;
      Ast_408.Parsetree.ptyexn_attributes = ptyexn_attributes }
     ->
    {
      Ast_407.Parsetree.ptyexn_constructor =
        (copy_Ast_408_Parsetree_extension_constructor ptyexn_constructor);
      Ast_407.Parsetree.ptyexn_loc = (copy_Ast_408_Location_t ptyexn_loc);
      Ast_407.Parsetree.ptyexn_attributes =
        (copy_Ast_408_Parsetree_attributes ptyexn_attributes)
    }*)

and copy_Ast_408_Parsetree_type_extension :
  Ast_408.Parsetree.type_extension -> Ast_407.Parsetree.type_extension =
  fun
    { Ast_408.Parsetree.ptyext_path = ptyext_path;
      Ast_408.Parsetree.ptyext_params = ptyext_params;
      Ast_408.Parsetree.ptyext_constructors = ptyext_constructors;
      Ast_408.Parsetree.ptyext_private = ptyext_private;
      Ast_408.Parsetree.ptyext_loc = _;
      Ast_408.Parsetree.ptyext_attributes = ptyext_attributes }
     ->
    {
      Ast_407.Parsetree.ptyext_path =
        (copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t ptyext_path);
      Ast_407.Parsetree.ptyext_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_Ast_408_Parsetree_core_type x0),
                (copy_Ast_408_Asttypes_variance x1))) ptyext_params);
      Ast_407.Parsetree.ptyext_constructors =
        (List.map copy_Ast_408_Parsetree_extension_constructor
           ptyext_constructors);
      Ast_407.Parsetree.ptyext_private =
        (copy_Ast_408_Asttypes_private_flag ptyext_private);
      Ast_407.Parsetree.ptyext_attributes =
        (copy_Ast_408_Parsetree_attributes ptyext_attributes)
    }

and copy_Ast_408_Parsetree_extension_constructor :
  Ast_408.Parsetree.extension_constructor ->
    Ast_407.Parsetree.extension_constructor
  =
  fun
    { Ast_408.Parsetree.pext_name = pext_name;
      Ast_408.Parsetree.pext_kind = pext_kind;
      Ast_408.Parsetree.pext_loc = pext_loc;
      Ast_408.Parsetree.pext_attributes = pext_attributes }
     ->
    {
      Ast_407.Parsetree.pext_name =
        (copy_Ast_408_Asttypes_loc (fun x  -> x) pext_name);
      Ast_407.Parsetree.pext_kind =
        (copy_Ast_408_Parsetree_extension_constructor_kind pext_kind);
      Ast_407.Parsetree.pext_loc = (copy_Ast_408_Location_t pext_loc);
      Ast_407.Parsetree.pext_attributes =
        (copy_Ast_408_Parsetree_attributes pext_attributes)
    }

and copy_Ast_408_Parsetree_extension_constructor_kind :
  Ast_408.Parsetree.extension_constructor_kind ->
    Ast_407.Parsetree.extension_constructor_kind
  =
  function
  | Ast_408.Parsetree.Pext_decl (x0,x1) ->
      Ast_407.Parsetree.Pext_decl
        ((copy_Ast_408_Parsetree_constructor_arguments x0),
          (copy_option copy_Ast_408_Parsetree_core_type x1))
  | Ast_408.Parsetree.Pext_rebind x0 ->
      Ast_407.Parsetree.Pext_rebind
        (copy_Ast_408_Asttypes_loc copy_Ast_408_Longident_t x0)

and copy_Ast_408_Parsetree_type_declaration :
  Ast_408.Parsetree.type_declaration -> Ast_407.Parsetree.type_declaration =
  fun
    { Ast_408.Parsetree.ptype_name = ptype_name;
      Ast_408.Parsetree.ptype_params = ptype_params;
      Ast_408.Parsetree.ptype_cstrs = ptype_cstrs;
      Ast_408.Parsetree.ptype_kind = ptype_kind;
      Ast_408.Parsetree.ptype_private = ptype_private;
      Ast_408.Parsetree.ptype_manifest = ptype_manifest;
      Ast_408.Parsetree.ptype_attributes = ptype_attributes;
      Ast_408.Parsetree.ptype_loc = ptype_loc }
     ->
    {
      Ast_407.Parsetree.ptype_name =
        (copy_Ast_408_Asttypes_loc (fun x  -> x) ptype_name);
      Ast_407.Parsetree.ptype_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_Ast_408_Parsetree_core_type x0),
                (copy_Ast_408_Asttypes_variance x1))) ptype_params);
      Ast_407.Parsetree.ptype_cstrs =
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              ((copy_Ast_408_Parsetree_core_type x0),
                (copy_Ast_408_Parsetree_core_type x1),
                (copy_Ast_408_Location_t x2))) ptype_cstrs);
      Ast_407.Parsetree.ptype_kind =
        (copy_Ast_408_Parsetree_type_kind ptype_kind);
      Ast_407.Parsetree.ptype_private =
        (copy_Ast_408_Asttypes_private_flag ptype_private);
      Ast_407.Parsetree.ptype_manifest =
        (copy_option copy_Ast_408_Parsetree_core_type ptype_manifest);
      Ast_407.Parsetree.ptype_attributes =
        (copy_Ast_408_Parsetree_attributes ptype_attributes);
      Ast_407.Parsetree.ptype_loc = (copy_Ast_408_Location_t ptype_loc)
    }

and copy_Ast_408_Asttypes_private_flag :
  Ast_408.Asttypes.private_flag -> Ast_407.Asttypes.private_flag =
  function
  | Ast_408.Asttypes.Private  -> Ast_407.Asttypes.Private
  | Ast_408.Asttypes.Public  -> Ast_407.Asttypes.Public

and copy_Ast_408_Parsetree_type_kind :
  Ast_408.Parsetree.type_kind -> Ast_407.Parsetree.type_kind =
  function
  | Ast_408.Parsetree.Ptype_abstract  -> Ast_407.Parsetree.Ptype_abstract
  | Ast_408.Parsetree.Ptype_variant x0 ->
      Ast_407.Parsetree.Ptype_variant
        (List.map copy_Ast_408_Parsetree_constructor_declaration x0)
  | Ast_408.Parsetree.Ptype_record x0 ->
      Ast_407.Parsetree.Ptype_record
        (List.map copy_Ast_408_Parsetree_label_declaration x0)
  | Ast_408.Parsetree.Ptype_open  -> Ast_407.Parsetree.Ptype_open

and copy_Ast_408_Parsetree_constructor_declaration :
  Ast_408.Parsetree.constructor_declaration ->
    Ast_407.Parsetree.constructor_declaration
  =
  fun
    { Ast_408.Parsetree.pcd_name = pcd_name;
      Ast_408.Parsetree.pcd_args = pcd_args;
      Ast_408.Parsetree.pcd_res = pcd_res;
      Ast_408.Parsetree.pcd_loc = pcd_loc;
      Ast_408.Parsetree.pcd_attributes = pcd_attributes }
     ->
    {
      Ast_407.Parsetree.pcd_name =
        (copy_Ast_408_Asttypes_loc (fun x  -> x) pcd_name);
      Ast_407.Parsetree.pcd_args =
        (copy_Ast_408_Parsetree_constructor_arguments pcd_args);
      Ast_407.Parsetree.pcd_res =
        (copy_option copy_Ast_408_Parsetree_core_type pcd_res);
      Ast_407.Parsetree.pcd_loc = (copy_Ast_408_Location_t pcd_loc);
      Ast_407.Parsetree.pcd_attributes =
        (copy_Ast_408_Parsetree_attributes pcd_attributes)
    }

and copy_Ast_408_Parsetree_constructor_arguments :
  Ast_408.Parsetree.constructor_arguments ->
    Ast_407.Parsetree.constructor_arguments
  =
  function
  | Ast_408.Parsetree.Pcstr_tuple x0 ->
      Ast_407.Parsetree.Pcstr_tuple
        (List.map copy_Ast_408_Parsetree_core_type x0)
  | Ast_408.Parsetree.Pcstr_record x0 ->
      Ast_407.Parsetree.Pcstr_record
        (List.map copy_Ast_408_Parsetree_label_declaration x0)

and copy_Ast_408_Parsetree_label_declaration :
  Ast_408.Parsetree.label_declaration -> Ast_407.Parsetree.label_declaration
  =
  fun
    { Ast_408.Parsetree.pld_name = pld_name;
      Ast_408.Parsetree.pld_mutable = pld_mutable;
      Ast_408.Parsetree.pld_type = pld_type;
      Ast_408.Parsetree.pld_loc = pld_loc;
      Ast_408.Parsetree.pld_attributes = pld_attributes }
     ->
    {
      Ast_407.Parsetree.pld_name =
        (copy_Ast_408_Asttypes_loc (fun x  -> x) pld_name);
      Ast_407.Parsetree.pld_mutable =
        (copy_Ast_408_Asttypes_mutable_flag pld_mutable);
      Ast_407.Parsetree.pld_type =
        (copy_Ast_408_Parsetree_core_type pld_type);
      Ast_407.Parsetree.pld_loc = (copy_Ast_408_Location_t pld_loc);
      Ast_407.Parsetree.pld_attributes =
        (copy_Ast_408_Parsetree_attributes pld_attributes)
    }

and copy_Ast_408_Asttypes_mutable_flag :
  Ast_408.Asttypes.mutable_flag -> Ast_407.Asttypes.mutable_flag =
  function
  | Ast_408.Asttypes.Immutable  -> Ast_407.Asttypes.Immutable
  | Ast_408.Asttypes.Mutable  -> Ast_407.Asttypes.Mutable

and copy_Ast_408_Asttypes_variance :
  Ast_408.Asttypes.variance -> Ast_407.Asttypes.variance =
  function
  | Ast_408.Asttypes.Covariant  -> Ast_407.Asttypes.Covariant
  | Ast_408.Asttypes.Contravariant  -> Ast_407.Asttypes.Contravariant
  | Ast_408.Asttypes.Invariant  -> Ast_407.Asttypes.Invariant

and copy_Ast_408_Parsetree_value_description :
  Ast_408.Parsetree.value_description -> Ast_407.Parsetree.value_description
  =
  fun
    { Ast_408.Parsetree.pval_name = pval_name;
      Ast_408.Parsetree.pval_type = pval_type;
      Ast_408.Parsetree.pval_prim = pval_prim;
      Ast_408.Parsetree.pval_attributes = pval_attributes;
      Ast_408.Parsetree.pval_loc = pval_loc }
     ->
    {
      Ast_407.Parsetree.pval_name =
        (copy_Ast_408_Asttypes_loc (fun x  -> x) pval_name);
      Ast_407.Parsetree.pval_type =
        (copy_Ast_408_Parsetree_core_type pval_type);
      Ast_407.Parsetree.pval_prim = (List.map (fun x  -> x) pval_prim);
      Ast_407.Parsetree.pval_attributes =
        (copy_Ast_408_Parsetree_attributes pval_attributes);
      Ast_407.Parsetree.pval_loc = (copy_Ast_408_Location_t pval_loc)
    }

and copy_Ast_408_Asttypes_arg_label :
  Ast_408.Asttypes.arg_label -> Ast_407.Asttypes.arg_label =
  function
  | Ast_408.Asttypes.Nolabel  -> Ast_407.Asttypes.Nolabel
  | Ast_408.Asttypes.Labelled x0 -> Ast_407.Asttypes.Labelled x0
  | Ast_408.Asttypes.Optional x0 -> Ast_407.Asttypes.Optional x0

and copy_Ast_408_Asttypes_closed_flag :
  Ast_408.Asttypes.closed_flag -> Ast_407.Asttypes.closed_flag =
  function
  | Ast_408.Asttypes.Closed  -> Ast_407.Asttypes.Closed
  | Ast_408.Asttypes.Open  -> Ast_407.Asttypes.Open

and copy_Ast_408_Asttypes_label :
  Ast_408.Asttypes.label -> Ast_407.Asttypes.label = fun x  -> x

and copy_Ast_408_Asttypes_rec_flag :
  Ast_408.Asttypes.rec_flag -> Ast_407.Asttypes.rec_flag =
  function
  | Ast_408.Asttypes.Nonrecursive  -> Ast_407.Asttypes.Nonrecursive
  | Ast_408.Asttypes.Recursive  -> Ast_407.Asttypes.Recursive

and copy_Ast_408_Parsetree_constant :
  Ast_408.Parsetree.constant -> Ast_407.Parsetree.constant =
  function
  | Ast_408.Parsetree.Pconst_integer (x0,x1) ->
      Ast_407.Parsetree.Pconst_integer (x0, (copy_option (fun x  -> x) x1))
  | Ast_408.Parsetree.Pconst_char x0 -> Ast_407.Parsetree.Pconst_char x0
  | Ast_408.Parsetree.Pconst_string (x0,x1) ->
      Ast_407.Parsetree.Pconst_string (x0, (copy_option (fun x  -> x) x1))
  | Ast_408.Parsetree.Pconst_float (x0,x1) ->
      Ast_407.Parsetree.Pconst_float (x0, (copy_option (fun x  -> x) x1))

and copy_option : 'f0 'g0 . ('f0 -> 'g0) -> 'f0 option -> 'g0 option =
  fun f0  -> function | None  -> None | Some x0 -> Some (f0 x0)

and copy_Ast_408_Longident_t : Ast_408.Longident.t -> Ast_407.Longident.t =
  function
  | Ast_408.Longident.Lident x0 -> Ast_407.Longident.Lident x0
  | Ast_408.Longident.Ldot (x0,x1) ->
      Ast_407.Longident.Ldot ((copy_Ast_408_Longident_t x0), x1)
  | Ast_408.Longident.Lapply (x0,x1) ->
      Ast_407.Longident.Lapply
        ((copy_Ast_408_Longident_t x0), (copy_Ast_408_Longident_t x1))

and copy_Ast_408_Asttypes_loc :
  'f0 'g0 .
    ('f0 -> 'g0) -> 'f0 Ast_408.Asttypes.loc -> 'g0 Ast_407.Asttypes.loc
  =
  fun f0  ->
    fun { Ast_408.Asttypes.txt = txt; Ast_408.Asttypes.loc = loc }  ->
      {
        Ast_407.Asttypes.txt = (f0 txt);
        Ast_407.Asttypes.loc = (copy_Ast_408_Location_t loc)
      }

and copy_Ast_408_Location_t : Ast_408.Location.t -> Ast_407.Location.t =
  fun
    { Ast_408.Location.loc_start = loc_start;
      Ast_408.Location.loc_end = loc_end;
      Ast_408.Location.loc_ghost = loc_ghost }
     ->
    {
      Ast_407.Location.loc_start = (copy_Lexing_position loc_start);
      Ast_407.Location.loc_end = (copy_Lexing_position loc_end);
      Ast_407.Location.loc_ghost = (copy_bool loc_ghost)
    }

and copy_bool : bool -> bool = function | false  -> false | true  -> true

and copy_Lexing_position : Lexing.position -> Lexing.position =
  fun
    { Lexing.pos_fname = pos_fname; Lexing.pos_lnum = pos_lnum;
      Lexing.pos_bol = pos_bol; Lexing.pos_cnum = pos_cnum }
     ->
    {
      Lexing.pos_fname = pos_fname;
      Lexing.pos_lnum = pos_lnum;
      Lexing.pos_bol = pos_bol;
      Lexing.pos_cnum = pos_cnum
    }
