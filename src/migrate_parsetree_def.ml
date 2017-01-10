type ocaml_version = [ `OCaml_402 | `OCaml_403 | `OCaml_404 ]

type ('intf, 'impl) intf_or_impl =
  | Intf of 'intf
  | Impl of 'impl

(** Errors that can happen when converting constructions that doesn't exist in
    older version of the AST. *)
type migration_error = [
  | `Pexp_letexception (** 4.04 -> 4.03: local exception, let exception _ in ... *)
  | `Ppat_open (** 4.04 -> 4.03: module open in pattern match x with M.(_) -> ... *)
  | `Pexp_unreachable (** 4.04 -> 4.03: unreachable pattern -> . *)
  | `PSig (** 4.03 -> 4.02: signature in attribute, [@: val x : int] *)
  | `Pcstr_record (** 4.03 -> 4.02: inline record *)
  | `Pconst_integer (** 4.03 -> 4.02: integer literal with invalid suffix, 1234d *)
  | `Pconst_float (** 4.03 -> 4.02: float literal with invalid suffix, 1234.0g *)
]

(* An compiler independent definition of location type, for reporting errors
   from any version of the AST. *)
type location = Location.t = {
  loc_start : Lexing.position;
  loc_end   : Lexing.position;
  loc_ghost : bool;
}

let location ~loc_start ~loc_end ~loc_ghost =
  { loc_start; loc_end; loc_ghost }

let location_none = Location.none

exception Migration_error of migration_error * location

let migration_error location error =
  raise (Migration_error (error,location))

let migration_error_feature = function
  | `Pexp_letexception -> "local exceptions"
  | `Ppat_open         -> "module open in patterns"
  | `Pexp_unreachable  -> "unreachable patterns"
  | `PSig              -> "signatures in attribute"
  | `Pcstr_record      -> "inline records"
  | `Pconst_integer    -> "custom integer literals"
  | `Pconst_float      -> "custom float literals"

let migration_error_supported_version = function
  | `Pexp_letexception -> `OCaml_404
  | `Ppat_open         -> `OCaml_404
  | `Pexp_unreachable  -> `OCaml_403
  | `PSig              -> `OCaml_403
  | `Pcstr_record      -> `OCaml_403
  | `Pconst_integer    -> `OCaml_403
  | `Pconst_float      -> `OCaml_403

let migration_error_message x =
  let feature = migration_error_feature x in
  let version = match migration_error_supported_version x with
    | `OCaml_404 -> "4.04"
    | `OCaml_403 -> "4.03"
  in
  feature ^ " are not supported before OCaml " ^ version

let location_prefix l =
  if l = location_none then ""
  else
    let {Location.loc_start; loc_end; _} = l in
    let bol = loc_start.Lexing.pos_bol in
    Printf.sprintf "File %S, line %d, characters %d-%d: "
      loc_start.Lexing.pos_fname
      loc_start.Lexing.pos_lnum
      (loc_start.Lexing.pos_cnum - bol)
      (loc_end.Lexing.pos_cnum - bol)

let () = Printexc.register_printer (function
    | Migration_error (err, loc) ->
        Some (location_prefix loc ^ migration_error_message err)
    | _ -> None
  )
