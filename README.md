# migrate-parsetree
Convert OCaml parsetrees between different major versions

This library converts between parsetrees of different OCaml versions.
It started from the work of Alain Frisch in [ppx\_tools](https://github.com/alainfrisch/ppx_tools).

Supported versions are 4.02, 4.03 and 4.04.
For each version, there is a snapshot of the parsetree and conversion functions
to the next and/or previous version.

## Asts

```ocaml
module Ast_402, Ast_403, Ast_404 : sig

  (* Copy of original type definitions *)
  module Location
  module Longident
  module Asttypes
  module Parsetree

  (* Magic numbers used for marshalling *)
  module Config : sig
    val ast_impl_magic_number : string
    val ast_intf_magic_number : string
  end

  (* Union of interface and implementation *)
  type ast =
    (Parsetree.signature, Parsetree.structure) Migrate_parsetree_def.intf_or_impl

  (* The current version of of the ast *)
  val version : [ `OCaml_402 | `OCaml_403 | `OCaml_404 ]

end
```

These embed copies of AST definitions of each supported OCaml major version.

The ast matching the version of the OCaml toolchain will use definitions from
compiler-libs.  For instance, when installed with OCaml 4.04.x, `Ast_404` looks
like:

```ocaml
module Location = Location
module Longident = Longident
module Asttypes = Asttypes
module Parsetree = Parsetree
module Config = Config

type ast =
  (Parsetree.signature, Parsetree.structure) Migrate_parsetree_def.intf_or_impl

let version : Migrate_parsetree_def.ocaml_version = `OCaml_404
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

# Development

The library includes code from different versions of OCaml. The resulting
license is not yet clear. Other code is licensed under MIT.

## Adding a new OCaml version

Snapshot the ast:
- Add a new constructor to
  [Migrate\_parsetree\_def.ocaml\_version](src/migrate_parsetree_def.ml)
- Create a file "asts/ast\_NEW.ml":
  * define the modules `Location`, `Longident`, `Asttypes`, `Parsetree` by
    keeping type definitions from the upstream files in `parsing/` directory
  * create a `Config` module containing `ast_impl_magic_number`
    `ast_impl_magic_number` from upstream `Config`
  * append 
```
type ast =
  (Parsetree.signature, Parsetree.structure) Migrate_parsetree_def.intf_or_impl

let version : Migrate_parsetree_def.ocaml_version = `OCaml_NEW
```

Add migration path:
- Manually compile the ast (`ocamlc -c ast_NEW.ml`)
- Using `gencopy` from [ppx\_tools](https://github.com/alainfrisch/ppx_tools), generate copy code to and from previous version (assuming it is 404):
```ocaml
gencopy -I . -map Ast_404:Ast_NEW Ast_404.Parsetree.expression > migrate_parsetree_404_NEW.ml
gencopy -I . -map Ast_NEW:Ast_404 Ast_NEW.Parsetree.expression > migrate_parsetree_NEW_404.ml
```
- Fix the generated code by implementing new cases
- By default generated code use very long identifiers, you can simplify unambiguous ones (e.g. `copy_Ast_NEW_Parsetree_structure` -> `copy_structure`)
- Update `Migrate_parsetree` module: add `migrate_to_NEW` function, implement missing cases

Update build system:
- in [Makefile](Makefile), add "src/ast\_NEW.ml" to `OCAML_ASTS` and migration modules to `OBJECTS`
- Update dependencies with `make depend`
- `make` should succeed
