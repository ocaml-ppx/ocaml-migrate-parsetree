(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                   Jérémie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(*$ #use "src/cinaps_helpers" $*)

(* Shared definitions.
   Mostly errors about features missing in older versions. *)
module Def = Migrate_parsetree_def

(* Copy of OCaml parsetrees *)
(*$foreach_version (fun suffix _ ->
    printf "module Ast_%s = Ast_%s\n" suffix suffix
  )*)
module Ast_402 = Ast_402
module Ast_403 = Ast_403
module Ast_404 = Ast_404
module Ast_405 = Ast_405
module Ast_406 = Ast_406
module Ast_407 = Ast_407
module Ast_408 = Ast_408
module Ast_409 = Ast_409
module Ast_410 = Ast_410
module Ast_411 = Ast_411
(*$*)

(* Manual migration between versions *)
(*$foreach_version_pair (fun x y ->
    printf "module Migrate_%s_%s = Migrate_parsetree_%s_%s_migrate\n" x y x y;
    printf "module Migrate_%s_%s = Migrate_parsetree_%s_%s_migrate\n" y x y x;
  )*)
module Migrate_402_403 = Migrate_parsetree_402_403_migrate
module Migrate_403_402 = Migrate_parsetree_403_402_migrate
module Migrate_403_404 = Migrate_parsetree_403_404_migrate
module Migrate_404_403 = Migrate_parsetree_404_403_migrate
module Migrate_404_405 = Migrate_parsetree_404_405_migrate
module Migrate_405_404 = Migrate_parsetree_405_404_migrate
module Migrate_405_406 = Migrate_parsetree_405_406_migrate
module Migrate_406_405 = Migrate_parsetree_406_405_migrate
module Migrate_406_407 = Migrate_parsetree_406_407_migrate
module Migrate_407_406 = Migrate_parsetree_407_406_migrate
module Migrate_407_408 = Migrate_parsetree_407_408_migrate
module Migrate_408_407 = Migrate_parsetree_408_407_migrate
module Migrate_408_409 = Migrate_parsetree_408_409_migrate
module Migrate_409_408 = Migrate_parsetree_409_408_migrate
module Migrate_409_410 = Migrate_parsetree_409_410_migrate
module Migrate_410_409 = Migrate_parsetree_410_409_migrate
module Migrate_410_411 = Migrate_parsetree_410_411_migrate
module Migrate_411_410 = Migrate_parsetree_411_410_migrate
(*$*)

(* Aliases for compiler-libs modules that might be shadowed *)
module Compiler_libs = struct
  module Location = Location
  module Longident = Longident

  module type Asttypes = module type of struct include Asttypes end
  module rec Asttypes : Asttypes = Asttypes

  module type Parsetree = module type of struct include Parsetree end
  module rec Parsetree : Parsetree = Parsetree

  module Docstrings = Docstrings
  module Ast_helper = Ast_helper
  module Ast_mapper = Ast_mapper
end
