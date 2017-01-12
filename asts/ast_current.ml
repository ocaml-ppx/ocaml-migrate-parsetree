module type Asttypes = module type of struct include Asttypes end
module type Parsetree = module type of struct include Parsetree end

module Location = Location
module Longident = Longident
module rec Asttypes : Asttypes = Asttypes
module rec Parsetree : Parsetree = Parsetree
module Config = Config

open Migrate_parsetree_def

type ast = (Parsetree.signature, Parsetree.structure) intf_or_impl

