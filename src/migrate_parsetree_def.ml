type migration_error = [
  | `Pexp_letexception (* 4.04 -> 4.03: let exception _ in ... *)
  | `Ppat_open (* 4.04 -> 4.03: match x with M.(_) -> ... *)

  | `Pexp_unreachable
  | `PSig
  | `Pcstr_record
  | `Pconst_integer
  | `Pconst_float
]

exception Migration_error of migration_error

let migration_error error =
  raise (Migration_error error)

let migration_error_message = function
  | `Pexp_letexception -> "4.04 -> 4.03: Pexp_letexception"
  | `Ppat_open         -> "4.04 -> 4.03: Ppat_open"
  | `Pexp_unreachable  -> "4.03 -> 4.02: Pexp_unreachable"
  | `PSig              -> "4.03 -> 4.02: PSig"
  | `Pcstr_record      -> "4.03 -> 4.02: Pcstr_record"
  | `Pconst_integer    -> "4.03 -> 4.02: Pconst_integer"
  | `Pconst_float      -> "4.03 -> 4.02: Pconst_float"

type ocaml_version = [`OCaml_402 | `OCaml_403 | `OCaml_404 ]

type ('intf, 'impl) intf_or_impl =
  | Intf of 'intf
  | Impl of 'impl
