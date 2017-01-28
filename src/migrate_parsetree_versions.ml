(* BEGIN of BLACK MAGIC *)
(*$define parsetree_types
  "structure" "signature" "toplevel_phrase" "core_type" "expression" "pattern"
  "case" "type_declaration" "type_extension" "extension_constructor" *)
(*$define outcometree_types
  "out_value" "out_type" "out_class_type" "out_module_type" "out_sig_item"
  "out_type_extension" "out_phrase"*)
(*$define all_types parsetree_types outcometree_types "mapper"*)

type _ witnesses = ..

module type Ast = sig
  module Parsetree : sig
    (*$foreach type parsetree_types "type " type "\n"*)
  end
  module Outcometree : sig
    (*$foreach type outcometree_types "type " type "\n"*)
  end
  module Ast_mapper : sig
    type mapper
  end
  module Config : sig
    val ast_impl_magic_number : string
    val ast_intf_magic_number : string
  end
end

type 'a _types = 'a constraint 'a = <
  (*$foreach type all_types type " : _;\n"*)
>

(*$foreach type all_types
   "type 'a get_"type" = 'x constraint 'a _types = < "type" : 'x; .. >\n"*)

module type OCaml_version = sig
  module Ast : Ast
  val string_version : string
  type types = <
    (*$foreach type parsetree_types " " type " : Ast.Parsetree." type ";\n"*)
    (*$foreach type outcometree_types " " type " : Ast.Outcometree." type ";\n"*)
    mapper : Ast.Ast_mapper.mapper;
    > _types
  type _ witnesses += Version : types witnesses
end

module Make_witness(Ast : Ast) =
struct
  type types = <
    (*$foreach type parsetree_types " " type " : Ast.Parsetree." type ";\n"*)
    (*$foreach type outcometree_types " " type " : Ast.Outcometree." type ";\n"*)
    mapper : Ast.Ast_mapper.mapper;
    > _types
  type _ witnesses += Version : types witnesses
end

type 'types ocaml_version =
  (module OCaml_version
   with type Ast.Ast_mapper.mapper = 'types get_mapper
    (*$foreach type parsetree_types
         "and type Ast.Parsetree." type " = 'types get_" type "\n"*)
    (*$foreach type outcometree_types
         "and type Ast.Outcometree." type " = 'types get_" type "\n"*)
  )

type an_ocaml_version = OCaml_version : 'a ocaml_version -> an_ocaml_version

module type Migration = sig
  module From : Ast
  module To : Ast
  (*$foreach type parsetree_types
     "val copy_" type " : From.Parsetree." type " -> To.Parsetree." type "\n"*)
  (*$foreach type outcometree_types
     "val copy_" type " : From.Outcometree." type " -> To.Outcometree." type "\n"*)
  val copy_mapper : From.Ast_mapper.mapper -> To.Ast_mapper.mapper
end

type ('from,'to_) migration =
  (module Migration
    with type From.Ast_mapper.mapper = 'from get_mapper
     and type To.Ast_mapper.mapper = 'to_ get_mapper
    (*$foreach type parsetree_types
         "and type " "From.Parsetree." type " = 'from get_" type "\n"*)
    (*$foreach type outcometree_types
         "and type " "From.Outcometree." type " = 'from get_" type "\n"*)
    (*$foreach type parsetree_types
         "and type " "To.Parsetree." type " = 'to_ get_" type "\n"*)
    (*$foreach type outcometree_types
         "and type " "To.Outcometree." type " = 'to_ get_" type "\n"*)
  )

module Id(Ast : Ast) : Migration with module From = Ast and module To = Ast = struct
  module From = Ast
  module To = Ast
  (*$foreach type all_types "let copy_" type " x = x\n"*)
end

module Compose(A : Migration)(B : Migration with module From = A.To) :
  Migration with module From = A.From and module To = B.To =
struct
  module From = A.From
  module To = B.To
  (*$foreach type all_types
      "let copy_" type " x = B.copy_" type " (A.copy_" type " x)\n"*)
end

type 'a chain =
    Chain : 'a ocaml_version * (('a,'b) migration * ('b,'a) migration * 'b chain) lazy_t -> 'a chain

type latest = Latest : 'a chain -> latest

module Make_conversion
    (Versions : sig val latest : latest end)
    (A : OCaml_version)(B : OCaml_version) :
  Migration with module From = A.Ast and module To = B.Ast =
struct
  module From = A.Ast
  module To = B.Ast

  type _ upcast = Upcast : ('a, 'b) migration * 'b chain -> 'a upcast
  type _ downcast = Downcast : ('b, 'a) migration * 'b chain -> 'a downcast

  let rec upcast : A.types upcast -> (A.types, B.types) migration =
    fun (Upcast ((module Mig) as mig, Chain ((module Ast'), chain))) ->
      match Ast'.Version, chain with
      | B.Version, _ -> mig
      | _, lazy ((module Up),_,chain') ->
          let module M = Compose(Mig)(Up) in
          upcast (Upcast ((module M),chain'))

  let rec downcast : B.types downcast -> (A.types, B.types) migration =
    fun (Downcast ((module Mig) as mig, Chain ((module Ast'), chain))) ->
      match Ast'.Version, chain with
      | A.Version, _ -> mig
      | _, lazy (_,(module Down),chain') ->
          let module M = Compose(Down)(Mig) in
          downcast (Downcast ((module M),chain'))

  let id (OCaml_version (module ML)) : (A.types, B.types) migration =
    let m : (ML.types,ML.types) migration = (module Id(ML.Ast)) in
    let m : (A.types,ML.types) migration =
      match A.Version with ML.Version -> m | _ -> assert false in
    let m : (A.types,B.types) migration =
      match B.Version with ML.Version -> m | _ -> assert false in
    m

  let rec select : type a . a chain -> (A.types, B.types) migration = function
    | Chain ((module ML), chain) ->
        match ML.Version, ML.Version, chain with
        | A.Version, B.Version, _ -> id (OCaml_version (module ML))
        | A.Version, _, lazy (up,_,chain') -> upcast (Upcast (up,chain'))
        | B.Version, _, lazy (_,down,chain') -> downcast (Downcast (down,chain'))
        | _, _, lazy (_,_,chain') -> select chain'

  module M = (val (let Latest chain = Versions.latest in select chain))
  (*$foreach type all_types
      "let copy_" type " = M.copy_" type "\n"*)
end

(* KNOWN VERSIONS *)

module OCaml_402 = struct
  module Ast = Ast_402
  include Make_witness(Ast_402)
  let string_version = "4.02"
end
module OCaml_403 = struct
  module Ast = Ast_403
  include Make_witness(Ast_403)
  let string_version = "4.03"
end
module OCaml_404 = struct
  module Ast = Ast_404
  include Make_witness(Ast_404)
  let string_version = "4.04"
end
module OCaml_405 = struct
  module Ast = Ast_405
  include Make_witness(Ast_405)
  let string_version = "4.05"
end

let all_versions : (module OCaml_version) list = [
  (module OCaml_402 : OCaml_version);
  (module OCaml_403 : OCaml_version);
  (module OCaml_404 : OCaml_version);
  (module OCaml_405 : OCaml_version);
]

let chain = Chain ((module OCaml_402), lazy (invalid_arg "Unknown Ast"))
let chain = Chain ((module OCaml_403), lazy ((module Migrate_parsetree_403_402),
                                             (module Migrate_parsetree_402_403), chain))
let chain = Chain ((module OCaml_404), lazy ((module Migrate_parsetree_404_403),
                                             (module Migrate_parsetree_403_404), chain))
let chain = Chain ((module OCaml_405), lazy ((module Migrate_parsetree_405_404),
                                             (module Migrate_parsetree_404_405), chain))

module Convert = Make_conversion(struct let latest = Latest chain end)

module OCaml_current = OCaml_(*$concat OCAML_VERSION*)

(* Make sure the preprocessing worked as expected *)
let _f (x : Parsetree.expression) : OCaml_current.Ast.Parsetree.expression = x
