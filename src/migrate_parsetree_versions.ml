(* BEGIN of BLACK MAGIC *)

type _ witnesses = ..

module type Ast = sig
  module Parsetree : sig
    type structure
    type signature
    type toplevel_phrase
    type core_type
    type expression
    type pattern
  end
  module Outcometree : sig
    type out_value
    type out_type
    type out_class_type
    type out_module_type
    type out_sig_item
    type out_type_extension
    type out_phrase
  end
  module Config : sig
    val ast_impl_magic_number : string
    val ast_intf_magic_number : string
  end
end

type 'a _types = 'a
  constraint 'a = <
  structure          : _;
  signature          : _;
  toplevel_phrase    : _;
  core_type          : _;
  expression         : _;
  pattern            : _;
  out_value          : _;
  out_type           : _;
  out_class_type     : _;
  out_module_type    : _;
  out_sig_item       : _;
  out_type_extension : _;
  out_phrase         : _;
>

type 'a get_structure          = 'x constraint 'a _types = < structure : 'x; .. >
type 'a get_signature          = 'x constraint 'a _types = < signature          : 'x; ..>
type 'a get_toplevel_phrase    = 'x constraint 'a _types = < toplevel_phrase    : 'x; ..>
type 'a get_core_type          = 'x constraint 'a _types = < core_type          : 'x; ..>
type 'a get_expression         = 'x constraint 'a _types = < expression         : 'x; ..>
type 'a get_pattern            = 'x constraint 'a _types = < pattern            : 'x; ..>
type 'a get_out_value          = 'x constraint 'a _types = < out_value          : 'x; ..>
type 'a get_out_type           = 'x constraint 'a _types = < out_type           : 'x; ..>
type 'a get_out_class_type     = 'x constraint 'a _types = < out_class_type     : 'x; ..>
type 'a get_out_module_type    = 'x constraint 'a _types = < out_module_type    : 'x; ..>
type 'a get_out_sig_item       = 'x constraint 'a _types = < out_sig_item       : 'x; ..>
type 'a get_out_type_extension = 'x constraint 'a _types = < out_type_extension : 'x; ..>
type 'a get_out_phrase         = 'x constraint 'a _types = < out_phrase         : 'x; ..>


module type OCaml_version = sig
  module Ast : Ast
  val string_version : string
  type types =
    < structure          : Ast.Parsetree.structure
    ; signature          : Ast.Parsetree.signature
    ; toplevel_phrase    : Ast.Parsetree.toplevel_phrase
    ; core_type          : Ast.Parsetree.core_type
    ; expression         : Ast.Parsetree.expression
    ; pattern            : Ast.Parsetree.pattern
    ; out_value          : Ast.Outcometree.out_value
    ; out_type           : Ast.Outcometree.out_type
    ; out_class_type     : Ast.Outcometree.out_class_type
    ; out_module_type    : Ast.Outcometree.out_module_type
    ; out_sig_item       : Ast.Outcometree.out_sig_item
    ; out_type_extension : Ast.Outcometree.out_type_extension
    ; out_phrase         : Ast.Outcometree.out_phrase
    > _types
  type _ witnesses += Version : types witnesses
end

module Make_witness(Raw : Ast) =
struct
  type types =
    < structure          : Raw.Parsetree.structure
    ; signature          : Raw.Parsetree.signature
    ; toplevel_phrase    : Raw.Parsetree.toplevel_phrase
    ; core_type          : Raw.Parsetree.core_type
    ; expression         : Raw.Parsetree.expression
    ; pattern            : Raw.Parsetree.pattern
    ; out_value          : Raw.Outcometree.out_value
    ; out_type           : Raw.Outcometree.out_type
    ; out_class_type     : Raw.Outcometree.out_class_type
    ; out_module_type    : Raw.Outcometree.out_module_type
    ; out_sig_item       : Raw.Outcometree.out_sig_item
    ; out_type_extension : Raw.Outcometree.out_type_extension
    ; out_phrase         : Raw.Outcometree.out_phrase
    > _types

  type _ witnesses += Version : types witnesses
end

type 'types ocaml_version =
  (module OCaml_version
    with type Ast.Parsetree.structure            = 'types get_structure
     and type Ast.Parsetree.signature            = 'types get_signature
     and type Ast.Parsetree.toplevel_phrase      = 'types get_toplevel_phrase
     and type Ast.Parsetree.core_type            = 'types get_core_type
     and type Ast.Parsetree.expression           = 'types get_expression
     and type Ast.Parsetree.pattern              = 'types get_pattern
     and type Ast.Outcometree.out_value          = 'types get_out_value
     and type Ast.Outcometree.out_type           = 'types get_out_type
     and type Ast.Outcometree.out_class_type     = 'types get_out_class_type
     and type Ast.Outcometree.out_module_type    = 'types get_out_module_type
     and type Ast.Outcometree.out_sig_item       = 'types get_out_sig_item
     and type Ast.Outcometree.out_type_extension = 'types get_out_type_extension
     and type Ast.Outcometree.out_phrase         = 'types get_out_phrase)

type an_ocaml_version = OCaml_version : 'a ocaml_version -> an_ocaml_version

module type Migration = sig
  module From : Ast
  module To : Ast
  val copy_structure          : From.Parsetree.structure             -> To.Parsetree.structure
  val copy_signature          : From.Parsetree.signature             -> To.Parsetree.signature
  val copy_toplevel_phrase    : From.Parsetree.toplevel_phrase       -> To.Parsetree.toplevel_phrase
  val copy_core_type          : From.Parsetree.core_type             -> To.Parsetree.core_type
  val copy_expression         : From.Parsetree.expression            -> To.Parsetree.expression
  val copy_pattern            : From.Parsetree.pattern               -> To.Parsetree.pattern
  val copy_out_value          : From.Outcometree.out_value           -> To.Outcometree.out_value
  val copy_out_type           : From.Outcometree.out_type            -> To.Outcometree.out_type
  val copy_out_class_type     : From.Outcometree.out_class_type      -> To.Outcometree.out_class_type
  val copy_out_module_type    : From.Outcometree.out_module_type     -> To.Outcometree.out_module_type
  val copy_out_sig_item       : From.Outcometree.out_sig_item        -> To.Outcometree.out_sig_item
  val copy_out_type_extension : From.Outcometree.out_type_extension  -> To.Outcometree.out_type_extension
  val copy_out_phrase         : From.Outcometree.out_phrase          -> To.Outcometree.out_phrase
end

type ('from,'to_) migration =
  (module Migration
    with type From.Parsetree.structure            = 'from get_structure
     and type From.Parsetree.signature            = 'from get_signature
     and type From.Parsetree.toplevel_phrase      = 'from get_toplevel_phrase
     and type From.Parsetree.core_type            = 'from get_core_type
     and type From.Parsetree.expression           = 'from get_expression
     and type From.Parsetree.pattern              = 'from get_pattern
     and type From.Outcometree.out_value          = 'from get_out_value
     and type From.Outcometree.out_type           = 'from get_out_type
     and type From.Outcometree.out_class_type     = 'from get_out_class_type
     and type From.Outcometree.out_module_type    = 'from get_out_module_type
     and type From.Outcometree.out_sig_item       = 'from get_out_sig_item
     and type From.Outcometree.out_type_extension = 'from get_out_type_extension
     and type From.Outcometree.out_phrase         = 'from get_out_phrase
     and type To.Parsetree.structure              = 'to_ get_structure
     and type To.Parsetree.signature              = 'to_ get_signature
     and type To.Parsetree.toplevel_phrase        = 'to_ get_toplevel_phrase
     and type To.Parsetree.core_type              = 'to_ get_core_type
     and type To.Parsetree.expression             = 'to_ get_expression
     and type To.Parsetree.pattern                = 'to_ get_pattern
     and type To.Outcometree.out_value            = 'to_ get_out_value
     and type To.Outcometree.out_type             = 'to_ get_out_type
     and type To.Outcometree.out_class_type       = 'to_ get_out_class_type
     and type To.Outcometree.out_module_type      = 'to_ get_out_module_type
     and type To.Outcometree.out_sig_item         = 'to_ get_out_sig_item
     and type To.Outcometree.out_type_extension   = 'to_ get_out_type_extension
     and type To.Outcometree.out_phrase           = 'to_ get_out_phrase)

module Id(Ast : Ast) : Migration with module From = Ast and module To = Ast = struct
  module From = Ast
  module To = Ast
  let copy_structure          x = x
  let copy_signature          x = x
  let copy_toplevel_phrase    x = x
  let copy_core_type          x = x
  let copy_expression         x = x
  let copy_pattern            x = x
  let copy_out_value          x = x
  let copy_out_type           x = x
  let copy_out_class_type     x = x
  let copy_out_module_type    x = x
  let copy_out_sig_item       x = x
  let copy_out_type_extension x = x
  let copy_out_phrase         x = x
end

module Compose(A : Migration)(B : Migration with module From = A.To) :
  Migration with module From = A.From and module To = B.To =
struct
  module From = A.From
  module To = B.To
  let copy_structure          x = B.copy_structure          (A.copy_structure          x)
  let copy_signature          x = B.copy_signature          (A.copy_signature          x)
  let copy_toplevel_phrase    x = B.copy_toplevel_phrase    (A.copy_toplevel_phrase    x)
  let copy_core_type          x = B.copy_core_type          (A.copy_core_type          x)
  let copy_expression         x = B.copy_expression         (A.copy_expression         x)
  let copy_pattern            x = B.copy_pattern            (A.copy_pattern            x)
  let copy_out_value          x = B.copy_out_value          (A.copy_out_value          x)
  let copy_out_type           x = B.copy_out_type           (A.copy_out_type           x)
  let copy_out_class_type     x = B.copy_out_class_type     (A.copy_out_class_type     x)
  let copy_out_module_type    x = B.copy_out_module_type    (A.copy_out_module_type    x)
  let copy_out_sig_item       x = B.copy_out_sig_item       (A.copy_out_sig_item       x)
  let copy_out_type_extension x = B.copy_out_type_extension (A.copy_out_type_extension x)
  let copy_out_phrase         x = B.copy_out_phrase         (A.copy_out_phrase         x)
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
  let copy_structure          = M.copy_structure
  let copy_signature          = M.copy_signature
  let copy_toplevel_phrase    = M.copy_toplevel_phrase
  let copy_core_type          = M.copy_core_type
  let copy_expression         = M.copy_expression
  let copy_pattern            = M.copy_pattern
  let copy_out_value          = M.copy_out_value
  let copy_out_type           = M.copy_out_type
  let copy_out_class_type     = M.copy_out_class_type
  let copy_out_module_type    = M.copy_out_module_type
  let copy_out_sig_item       = M.copy_out_sig_item
  let copy_out_type_extension = M.copy_out_type_extension
  let copy_out_phrase         = M.copy_out_phrase
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

module OCaml_current = OCaml_OCAML_VERSION

(* Make sure the preprocessing worked as expected *)
let _f (x : Parsetree.expression) : OCaml_current.Ast.Parsetree.expression = x
