module Location = Location
module Longident = Longident
module Asttypes = Asttypes
module Parsetree = Parsetree

let ast_impl_magic_number = Config.ast_impl_magic_number
let ast_intf_magic_number = Config.ast_intf_magic_number

type ast =
  | Intf of Parsetree.signature
  | Impl of Parsetree.structure

