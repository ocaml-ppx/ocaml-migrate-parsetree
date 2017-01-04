# migrate-parsetree
Convert OCaml parsetrees between different major versions

This library converts between parsetrees of different OCaml versions.
It exposes two series of modules.

## Frontends

```ocaml
module Frontend_402, Frontend_403, Frontend_404 : sig

  (* Copy of original type definitions *)
  module Location
  module Longident
  module Asttypes
  module Parsetree

  (* Magic numbers used for marshalling *)
  val ast_impl_magic_number : string
  val ast_intf_magic_number : string

  (* Union of interface and implementation *)
  type ast =
    | Intf of Parsetree.signature
    | Impl of Parsetree.structure

  (* The current version of of the frontend *)
  val ocaml_version : [ `OCaml_402 | `OCaml_403 | `OCaml_404 ]
end
```

These embed copies of AST definitions of each supported OCaml major version.

For the frontend matching the version of the OCaml toolchain aliases to
compiler-libs are used instead of copy.  For instance, when installed with
OCaml 4.04.x, `Frontend_404` looks like:

```ocaml
module Location = Location
module Longident = Longident
module Asttypes = Asttypes
module Parsetree = Parsetree

let ast_impl_magic_number = Config.ast_impl_magic_number
let ast_intf_magic_number = Config.ast_intf_magic_number

type ast =
  | Intf of Parsetree.signature
  | Impl of Parsetree.structure

let ocaml_version : Migrate_parsetree_def.ocaml_version = `OCaml_404
```

## Migration modules

For each pair of versions `$(n)` and `$(n+1)`, the two modules
`Migrate_parsetree_$(n)_$(n+1)` and `Migrate_parsetree_$(n+1)_$(n)` convert the AST forward and backward.

The forward conversion is total while the backward conversion is partial: when
a feature is not available in a previous version of the parsetree, a
`Migrate_parsetree_def.Migration_error` exception is raised detailing the
failure case.

Finally, the `Migrate_parsetree` module exposes an easy interface for
transparent (un)marshalling and conversion between any supported version.
