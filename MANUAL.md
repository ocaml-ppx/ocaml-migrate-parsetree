Title:  Guide to OCaml Migrate Parsetree
Author: Frédéric Bour, @let-def
Date:   March 9, 2017


**Table of Contents**

- [Manipulating parsetree](#manipulating-parsetree)
  - [Migrating between compiler versions](#migrating-between-compiler-versions)
- [ppx_tools_versioned](#ppx_tools_versioned)
    - [ppx_metaquots](#ppx_metaquots)
- [Troubleshooting](#troubleshooting)
  - [Accessing shadowed compiler libs module](#accessing-shadowed-compiler-libs-module)
  - [Using functions from compiler-libs results in (unfriendly) type errors](#using-functions-from-compiler-libs-results-in-unfriendly-type-errors)
  - [Features not supported in targeted version](#features-not-supported-in-targeted-version)
    - [What kind of guarantees to expect in practice?](#what-kind-of-guarantees-to-expect-in-practice)

This library is designed to make PPX rewriters portable across compiler versions.

It works by versioning the definitions of OCaml AST. This includes `Parsetree`, `Asttypes`, `Outcometree`, `Ast_helper` and most of `Docstrings` and `Ast_mapper`.

*Note:* `Docstrings` and `Ast_mapper` contain some global state which was removed during versioning. This affect registration of rewriters when using `Ast_mapper` as a driver. See the [driver section](#drivers) for reliable solutions.

# Manipulating parsetree

Most of the work happens by shadowing. If your PPX rewriter was written against OCaml 4.04 AST, just `open Ast_404` (alternatively, you can pass `-open Ast_404` when building the file).

This will introduce the versioned modules in scope. When compiled with other supported versions of OCaml, the definitions are still compatible with 4.04.

While this is enough to manipulate the AST from within your code, you can no longer have expectations on the version of `compiler-libs`. The rest of the `Migrate_parsetree` module provides tools to deal with that.

## Migrating between compiler versions

When migrating between two known compiler versions, the modules `Migrate_parsetree.Migrate_40x_40y` contain functions to transform values between two consecutive versions.

For instance `Migrate_402_403.copy_signature` turns a signature of OCaml 4.02 into a signature for OCaml 4.03. `Migrate_404_403.copy_mapper` transforms an `Ast_mapper.mapper` for OCaml 4.04 into a mapper for OCaml 4.03.

When working with an arbitrary version, it becomes useful to quantify over versions and migrations. The `Migrate_parsetree.Versions` module comes again to the rescue.

The `migrate_functions` record is a list of functions for converting each type.

# ppx_tools_versioned

Some rewriters make use of the *ppx_tools* package that offers conveniences for manipulating parsetrees.  As *ppx_tools* itself uses compiler-libs, using it directly defeats the purpose of *ocaml-migrate-parsetree*.

We provide the [ppx_tools_versioned](https://github.com/let-def/ppx_tools_versioned) package to overcome this. It offers migrate friendly versions of `Ast_convenience`, `Ast_lifter`, `Ast_mapper_class` and `Ppx_metaquot`.

To use these versions, just append `_40x` to the module names or `open Ppx_tool_40x` module.

```ocaml
(* Original code *)
open Ast_mapper_class

class my_mapper =
  object
    inherit mapper
    ...
end

(* Targeting 4.04 *)
open Ast_404
open Ppx_tools_404

open Ast_mapper_class

class my_mapper =
  object
    inherit mapper
    ...
end

(* Alternatively, if you use a single module from Ppx_tools *)
open Ast_mapper_class_404

class my_mapper =
  object
    inherit mapper
    ...
end
```

### ppx_metaquots

The *metaquot* rewriter allows quoting of the OCaml AST. The version provided by *ppx_tools* will quote the Parsetree from *compiler-libs*.

The versioned ones are accessed by using *ppx_tools_versioned.metaquot_40x* packages.

For instance, *ppx_tools_versioned.metaquot_404* will quote `Ast_404.Parsetree`.

# Troubleshooting

## Accessing shadowed compiler libs module

`Migrate_parsetree` defines a `Compiler_libs` module that reexports all modules that could have been shadowed by `Ast_40x` modules.

## Using functions from compiler-libs results in (unfriendly) type errors

Remember that because of abstraction, most values manipulated from within the rewriter have types that are unrelated to compiler-libs definitions.

For instance, you cannot directly use `Pprintast.core_type` to print a type. You should first make a migration record for the version you are targeting and then lift the `core_type` instance:

```ocaml
(* Assuming rewriter is written against OCaml 4.04 parsetree *)
let migration =
  Versions.migrate Versions.ocaml_404 Versions.ocaml_current

let print_core_type fmt typ =
  Pprintast.core_type fmt (migration.copy_core_type typ)
```

As for the error message, it contains all information needed to be polymorphic over a whole version of compiler parsetree. Pick what is relevant to your use case :-).

## Features not supported in targeted version

When converting to an earlier version, some features might not be supported. In this case, the migration library will raise an exception. You can find the definition of these cases in `Migrate_parsetree.Def`.

A reasonable error message is provided by default, otherwise you should catch `Migration_error` exceptions after any call to a migration function  (either a call to a function from `Migrate_40x_40y` or to a field of `migrate_functions` record). Only backward migrations are partials.

### What kind of guarantees to expect in practice?

The fact that migrations are partial functions can seem too restrictive.
In practice, a problem only happens when an OCaml construction is used that didn't exist in the version the PPX rewriter was implemented with.

This cannot occur when a new version of the compiler is released: existing code that was working before should work immediately after an update, since new features are not yet in use. This use case is the critical one for helping the introduction of a new compiler version (an opam switch should be usable readily after update).

In the future, we might allow rewriting of unsupported features into extensions or attributes for rewriters that opt-in. Rewriting would succeed as long as all extensions disappeared when reaching the compiler (for instance, an OCaml 4.04 file using inline records could be rewritten by a rewriter targeting 4.02; however, a 4.02 files couldn't be rewritten by a 4.04 PPX that introduces inline records).

Please voice your concerns if you have any, so that this use case is better understood.4.02
