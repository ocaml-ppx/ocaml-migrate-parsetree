# OCaml-migrate-parsetree
Convert OCaml parsetrees between different major versions

This library converts between parsetrees of different OCaml versions.

Supported versions are 4.02, 4.03, 4.04, 4.05, 4.06, 4.07, 4.08, 4.09,
4.10, and 4.11. For each version, there is a snapshot of the parsetree
and conversion functions to the next and/or previous version.

## Asts

```ocaml
module Ast_{402,403,404,405,406,407,408,409,410,411} : sig

  (* These two modules didn't change between compiler versions.
     Just share the ones from compiler-libs. *)
  module Location = Location
  module Longident = Longident

  (* Version specific copy of AST *)
  module Asttypes
  module Parsetree
  module Outcometree

  (* Other modules that are useful for implementing PPX.

     Docstrings and Ast_mapper only contain general definitions
     In particular, the internal state used by compiler-libs has been
     removed.
     Also equalities are lost for abstract types (Docstring.docstring).  *)
  module Docstrings
  module Ast_helper
  module Ast_mapper

  (* Magic numbers used for marshalling *)
  module Config : sig
    val ast_impl_magic_number : string
    val ast_intf_magic_number : string
  end
end
```

These embed copies of AST definitions for each supported OCaml major version.

The AST matching the version of the OCaml toolchain will contain equalities
relating the copy of types to the definitions from compiler-libs.  For
instance, when installed with OCaml 4.04.x, `Ast_404.Parsetree` looks
like.

## Migration modules

For each pair of versions `$(n)` and `$(n+1)`, the two modules
`Migrate_parsetree_$(n)_$(n+1)` and `Migrate_parsetree_$(n+1)_$(n)` convert the AST forward and backward.

The forward conversion is total while the backward conversion is partial: when
a feature is not available in a previous version of the parsetree, a
`Migrate_parsetree_def.Migration_error` exception is raised detailing the
failure case.

## Adding a new OCaml version

We use [Cinaps](https://github.com/janestreet/cinaps) to generate boilerplate.
You can install it via opam: `opam install cinaps`.

Add the new version in
[src/cinaps_helpers](https://github.com/ocaml-ppx/ocaml-migrate-parsetree/blob/master/src/cinaps_helpers)
`supported_versions`.

Copy the last `src/ast_xxx.ml` file to `src/ast_<new_version>.ml`,
then go over the file and update each sub-module by replacing its
signature and implementation with the code from the compiler. For the
`Config` sub-module, update the two variables with the values in
`utils/config.mlp` in the compiler source tree.

Once this is done, call:

    $ dune exec tools/add_special_comments.exe src/ast_<new_version>.ml

Then diff the `src/ast_xxx.ml` and `src/ast_<new_version>.ml` and go
over the diff to make sure the difference are relevant. The `ast_...`
files require some adjustments which should pop up when you do this
diff. Port the old adjustments to the new file as required.

Add migration functions:
- Manually compile the asts (`ocamlc -c src/ast_{NEW,OLD}.ml -I +compiler-libs -I _build/default/src/.migrate_parsetree.objs/byte/ -open Migrate_parsetree__`)
- Using `tools/gencopy.exe` (`dune build tools/gencopy.exe`), generate copy code to and from previous version (assuming it is 408):
```
_build/default/tools/gencopy.exe -I . -I src/ -I +compiler-libs -map Ast_409:Ast_408 Ast_409.Parsetree.{expression,expr,pattern,pat,core_type,typ,toplevel_phrase} Ast_409.Outcometree.{out_phrase,out_type_extension} > src/migrate_parsetree_409_408_migrate.ml
_build/default/tools/gencopy.exe -I . -I src/ -I +compiler-libs -map Ast_408:Ast_409 Ast_408.Parsetree.{expression,expr,pattern,pat,core_type,typ,toplevel_phrase} Ast_408.Outcometree.{out_phrase,out_type_extension} > src/migrate_parsetree_408_409_migrate.ml
```
- Fix the generated code by implementing new cases

*TODO*: specialize and improve gencopy for these cases

At any time, you can expand boilerplate code by running `make cinaps`.

Update build system:
- make sure `make cinaps` reaches a fixed point :)
- `make` should succeed
