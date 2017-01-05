module Location = Location
module Longident = Longident
module Asttypes = Asttypes
module Parsetree = Parsetree
module Config = Config

type ast =
  (Parsetree.signature, Parsetree.structure) Migrate_parsetree_def.intf_or_impl

