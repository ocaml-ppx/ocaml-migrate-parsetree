type ocaml_version =
  | OCaml_402
  | OCaml_403
  | OCaml_404
  | OCaml_405

type ('intf, 'impl) intf_or_impl =
  | Intf of 'intf
  | Impl of 'impl

(** Errors that can happen when converting constructions that doesn't exist in
    older version of the AST. *)
type missing_feature =
  | Pexp_letexception
    (** 4.04 -> 4.03: local exception, let exception _ in ... *)
  | Ppat_open
    (** 4.04 -> 4.03: module open in pattern match x with M.(_) -> ... *)
  | Pexp_unreachable
    (** 4.04 -> 4.03: unreachable pattern -> . *)
  | PSig
    (** 4.03 -> 4.02: signature in attribute, [@: val x : int] *)
  | Pcstr_record
    (** 4.03 -> 4.02: inline record *)
  | Pconst_integer
    (** 4.03 -> 4.02: integer literal with invalid suffix, 1234d *)
  | Pconst_float
    (** 4.03 -> 4.02: float literal with invalid suffix, 1234.0g *)

(* An compiler independent definition of location type, for reporting errors
   from any version of the AST. *)
type location = Location.t = {
  loc_start : Lexing.position;
  loc_end   : Lexing.position;
  loc_ghost : bool;
}

let location ~loc_start ~loc_end ~loc_ghost =
  { loc_start; loc_end; loc_ghost }

exception Migration_error of missing_feature * location
