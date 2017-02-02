(* BEGIN of BLACK MAGIC *)
(*$ #use "src/cinaps.ml" $*)

type _ witnesses = ..

module type Ast = sig
  (*$ foreach_module (fun m types ->
        printf "module %s : sig\n" m;
        List.iter types ~f:(printf "type %s\n");
        printf "end\n"
      )
  *)
  module Parsetree : sig
    type structure
    type signature
    type toplevel_phrase
    type core_type
    type expression
    type pattern
    type case
    type type_declaration
    type type_extension
    type extension_constructor
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
  module Ast_mapper : sig
    type mapper
  end
  (*$*)
  module Config : sig
    val ast_impl_magic_number : string
    val ast_intf_magic_number : string
  end
end

type 'a _types = 'a constraint 'a
  = <
  (*$ foreach_type (fun _ s -> printf "%-21s : _;\n" s) *)

  structure             : _;
  signature             : _;
  toplevel_phrase       : _;
  core_type             : _;
  expression            : _;
  pattern               : _;
  case                  : _;
  type_declaration      : _;
  type_extension        : _;
  extension_constructor : _;
  out_value             : _;
  out_type              : _;
  out_class_type        : _;
  out_module_type       : _;
  out_sig_item          : _;
  out_type_extension    : _;
  out_phrase            : _;
  mapper                : _;
  (*$*)
  >
;;

(*$ foreach_type (fun _ s ->
      printf "type 'a get_%s =\n" s;
      printf " 'x constraint 'a _types = < %s : 'x; .. >\n" s
    ) *)

type 'a get_structure =
  'x constraint 'a _types = < structure : 'x; .. >
type 'a get_signature =
  'x constraint 'a _types = < signature : 'x; .. >
type 'a get_toplevel_phrase =
  'x constraint 'a _types = < toplevel_phrase : 'x; .. >
type 'a get_core_type =
  'x constraint 'a _types = < core_type : 'x; .. >
type 'a get_expression =
  'x constraint 'a _types = < expression : 'x; .. >
type 'a get_pattern =
  'x constraint 'a _types = < pattern : 'x; .. >
type 'a get_case =
  'x constraint 'a _types = < case : 'x; .. >
type 'a get_type_declaration =
  'x constraint 'a _types = < type_declaration : 'x; .. >
type 'a get_type_extension =
  'x constraint 'a _types = < type_extension : 'x; .. >
type 'a get_extension_constructor =
  'x constraint 'a _types = < extension_constructor : 'x; .. >
type 'a get_out_value =
  'x constraint 'a _types = < out_value : 'x; .. >
type 'a get_out_type =
  'x constraint 'a _types = < out_type : 'x; .. >
type 'a get_out_class_type =
  'x constraint 'a _types = < out_class_type : 'x; .. >
type 'a get_out_module_type =
  'x constraint 'a _types = < out_module_type : 'x; .. >
type 'a get_out_sig_item =
  'x constraint 'a _types = < out_sig_item : 'x; .. >
type 'a get_out_type_extension =
  'x constraint 'a _types = < out_type_extension : 'x; .. >
type 'a get_out_phrase =
  'x constraint 'a _types = < out_phrase : 'x; .. >
type 'a get_mapper =
  'x constraint 'a _types = < mapper : 'x; .. >
       (*$*)

module type OCaml_version = sig
  module Ast : Ast
  val string_version : string
  type types = <
    (*$ foreach_type (fun m s -> printf "%-21s : Ast.%s.%s;\n" s m s)*)

    structure             : Ast.Parsetree.structure;
    signature             : Ast.Parsetree.signature;
    toplevel_phrase       : Ast.Parsetree.toplevel_phrase;
    core_type             : Ast.Parsetree.core_type;
    expression            : Ast.Parsetree.expression;
    pattern               : Ast.Parsetree.pattern;
    case                  : Ast.Parsetree.case;
    type_declaration      : Ast.Parsetree.type_declaration;
    type_extension        : Ast.Parsetree.type_extension;
    extension_constructor : Ast.Parsetree.extension_constructor;
    out_value             : Ast.Outcometree.out_value;
    out_type              : Ast.Outcometree.out_type;
    out_class_type        : Ast.Outcometree.out_class_type;
    out_module_type       : Ast.Outcometree.out_module_type;
    out_sig_item          : Ast.Outcometree.out_sig_item;
    out_type_extension    : Ast.Outcometree.out_type_extension;
    out_phrase            : Ast.Outcometree.out_phrase;
    mapper                : Ast.Ast_mapper.mapper;
    (*$*)
  > _types
  type _ witnesses += Version : types witnesses
end

module Make_witness(Ast : Ast) =
struct
  type types = <
    (*$ foreach_type (fun m s -> printf "%-21s : Ast.%s.%s;\n" s m s)*)

    structure             : Ast.Parsetree.structure;
    signature             : Ast.Parsetree.signature;
    toplevel_phrase       : Ast.Parsetree.toplevel_phrase;
    core_type             : Ast.Parsetree.core_type;
    expression            : Ast.Parsetree.expression;
    pattern               : Ast.Parsetree.pattern;
    case                  : Ast.Parsetree.case;
    type_declaration      : Ast.Parsetree.type_declaration;
    type_extension        : Ast.Parsetree.type_extension;
    extension_constructor : Ast.Parsetree.extension_constructor;
    out_value             : Ast.Outcometree.out_value;
    out_type              : Ast.Outcometree.out_type;
    out_class_type        : Ast.Outcometree.out_class_type;
    out_module_type       : Ast.Outcometree.out_module_type;
    out_sig_item          : Ast.Outcometree.out_sig_item;
    out_type_extension    : Ast.Outcometree.out_type_extension;
    out_phrase            : Ast.Outcometree.out_phrase;
    mapper                : Ast.Ast_mapper.mapper;
    (*$*)
  > _types
  type _ witnesses += Version : types witnesses
end

type 'types ocaml_version =
  (module OCaml_version
    (*$ let sep = with_then_and () in
        foreach_type (fun m s ->
          printf "%t type Ast.%s.%s = 'types get_%s\n" sep m s s) *)

    with type Ast.Parsetree.structure = 'types get_structure
     and type Ast.Parsetree.signature = 'types get_signature
     and type Ast.Parsetree.toplevel_phrase = 'types get_toplevel_phrase
     and type Ast.Parsetree.core_type = 'types get_core_type
     and type Ast.Parsetree.expression = 'types get_expression
     and type Ast.Parsetree.pattern = 'types get_pattern
     and type Ast.Parsetree.case = 'types get_case
     and type Ast.Parsetree.type_declaration = 'types get_type_declaration
     and type Ast.Parsetree.type_extension = 'types get_type_extension
     and type Ast.Parsetree.extension_constructor = 'types get_extension_constructor
     and type Ast.Outcometree.out_value = 'types get_out_value
     and type Ast.Outcometree.out_type = 'types get_out_type
     and type Ast.Outcometree.out_class_type = 'types get_out_class_type
     and type Ast.Outcometree.out_module_type = 'types get_out_module_type
     and type Ast.Outcometree.out_sig_item = 'types get_out_sig_item
     and type Ast.Outcometree.out_type_extension = 'types get_out_type_extension
     and type Ast.Outcometree.out_phrase = 'types get_out_phrase
     and type Ast.Ast_mapper.mapper = 'types get_mapper
     (*$*)
  )

type an_ocaml_version = OCaml_version : 'a ocaml_version -> an_ocaml_version

module type Migration = sig
  module From : Ast
  module To : Ast
  (*$ foreach_type (fun m s ->
        printf "val copy_%s: From.%s.%s -> To.%s.%s\n" s m s m s) *)

  val copy_structure: From.Parsetree.structure -> To.Parsetree.structure
  val copy_signature: From.Parsetree.signature -> To.Parsetree.signature
  val copy_toplevel_phrase: From.Parsetree.toplevel_phrase -> To.Parsetree.toplevel_phrase
  val copy_core_type: From.Parsetree.core_type -> To.Parsetree.core_type
  val copy_expression: From.Parsetree.expression -> To.Parsetree.expression
  val copy_pattern: From.Parsetree.pattern -> To.Parsetree.pattern
  val copy_case: From.Parsetree.case -> To.Parsetree.case
  val copy_type_declaration: From.Parsetree.type_declaration -> To.Parsetree.type_declaration
  val copy_type_extension: From.Parsetree.type_extension -> To.Parsetree.type_extension
  val copy_extension_constructor: From.Parsetree.extension_constructor -> To.Parsetree.extension_constructor
  val copy_out_value: From.Outcometree.out_value -> To.Outcometree.out_value
  val copy_out_type: From.Outcometree.out_type -> To.Outcometree.out_type
  val copy_out_class_type: From.Outcometree.out_class_type -> To.Outcometree.out_class_type
  val copy_out_module_type: From.Outcometree.out_module_type -> To.Outcometree.out_module_type
  val copy_out_sig_item: From.Outcometree.out_sig_item -> To.Outcometree.out_sig_item
  val copy_out_type_extension: From.Outcometree.out_type_extension -> To.Outcometree.out_type_extension
  val copy_out_phrase: From.Outcometree.out_phrase -> To.Outcometree.out_phrase
  val copy_mapper: From.Ast_mapper.mapper -> To.Ast_mapper.mapper
  (*$*)
end

type ('from,'to_) migration =
  (module Migration
    (*$ let sep = with_then_and () in
        foreach_type (fun m s ->
          let fq = m ^ "." ^ s in
          printf "    %t type From.%-31s = 'from get_%s\n" sep fq s;
          printf "    %t type   To.%-31s = 'to_  get_%s\n" sep fq s) *)

    with type From.Parsetree.structure             = 'from get_structure
     and type   To.Parsetree.structure             = 'to_  get_structure
     and type From.Parsetree.signature             = 'from get_signature
     and type   To.Parsetree.signature             = 'to_  get_signature
     and type From.Parsetree.toplevel_phrase       = 'from get_toplevel_phrase
     and type   To.Parsetree.toplevel_phrase       = 'to_  get_toplevel_phrase
     and type From.Parsetree.core_type             = 'from get_core_type
     and type   To.Parsetree.core_type             = 'to_  get_core_type
     and type From.Parsetree.expression            = 'from get_expression
     and type   To.Parsetree.expression            = 'to_  get_expression
     and type From.Parsetree.pattern               = 'from get_pattern
     and type   To.Parsetree.pattern               = 'to_  get_pattern
     and type From.Parsetree.case                  = 'from get_case
     and type   To.Parsetree.case                  = 'to_  get_case
     and type From.Parsetree.type_declaration      = 'from get_type_declaration
     and type   To.Parsetree.type_declaration      = 'to_  get_type_declaration
     and type From.Parsetree.type_extension        = 'from get_type_extension
     and type   To.Parsetree.type_extension        = 'to_  get_type_extension
     and type From.Parsetree.extension_constructor = 'from get_extension_constructor
     and type   To.Parsetree.extension_constructor = 'to_  get_extension_constructor
     and type From.Outcometree.out_value           = 'from get_out_value
     and type   To.Outcometree.out_value           = 'to_  get_out_value
     and type From.Outcometree.out_type            = 'from get_out_type
     and type   To.Outcometree.out_type            = 'to_  get_out_type
     and type From.Outcometree.out_class_type      = 'from get_out_class_type
     and type   To.Outcometree.out_class_type      = 'to_  get_out_class_type
     and type From.Outcometree.out_module_type     = 'from get_out_module_type
     and type   To.Outcometree.out_module_type     = 'to_  get_out_module_type
     and type From.Outcometree.out_sig_item        = 'from get_out_sig_item
     and type   To.Outcometree.out_sig_item        = 'to_  get_out_sig_item
     and type From.Outcometree.out_type_extension  = 'from get_out_type_extension
     and type   To.Outcometree.out_type_extension  = 'to_  get_out_type_extension
     and type From.Outcometree.out_phrase          = 'from get_out_phrase
     and type   To.Outcometree.out_phrase          = 'to_  get_out_phrase
     and type From.Ast_mapper.mapper               = 'from get_mapper
     and type   To.Ast_mapper.mapper               = 'to_  get_mapper
     (*$*)
  )

module Id(Ast : Ast) : Migration with module From = Ast and module To = Ast = struct
  module From = Ast
  module To = Ast
  (*$ foreach_type (fun _ s -> printf "let copy_%-21s x = x\n" s) *)

  let copy_structure             x = x
  let copy_signature             x = x
  let copy_toplevel_phrase       x = x
  let copy_core_type             x = x
  let copy_expression            x = x
  let copy_pattern               x = x
  let copy_case                  x = x
  let copy_type_declaration      x = x
  let copy_type_extension        x = x
  let copy_extension_constructor x = x
  let copy_out_value             x = x
  let copy_out_type              x = x
  let copy_out_class_type        x = x
  let copy_out_module_type       x = x
  let copy_out_sig_item          x = x
  let copy_out_type_extension    x = x
  let copy_out_phrase            x = x
  let copy_mapper                x = x
  (*$*)
end

module Compose(A : Migration)(B : Migration with module From = A.To) :
  Migration with module From = A.From and module To = B.To =
struct
  module From = A.From
  module To = B.To
  (*$ foreach_type (fun _ s ->
        printf "let copy_%-21s x = B.copy_%-21s (A.copy_%-21s x)\n" s s s) *)

  let copy_structure             x = B.copy_structure             (A.copy_structure             x)
  let copy_signature             x = B.copy_signature             (A.copy_signature             x)
  let copy_toplevel_phrase       x = B.copy_toplevel_phrase       (A.copy_toplevel_phrase       x)
  let copy_core_type             x = B.copy_core_type             (A.copy_core_type             x)
  let copy_expression            x = B.copy_expression            (A.copy_expression            x)
  let copy_pattern               x = B.copy_pattern               (A.copy_pattern               x)
  let copy_case                  x = B.copy_case                  (A.copy_case                  x)
  let copy_type_declaration      x = B.copy_type_declaration      (A.copy_type_declaration      x)
  let copy_type_extension        x = B.copy_type_extension        (A.copy_type_extension        x)
  let copy_extension_constructor x = B.copy_extension_constructor (A.copy_extension_constructor x)
  let copy_out_value             x = B.copy_out_value             (A.copy_out_value             x)
  let copy_out_type              x = B.copy_out_type              (A.copy_out_type              x)
  let copy_out_class_type        x = B.copy_out_class_type        (A.copy_out_class_type        x)
  let copy_out_module_type       x = B.copy_out_module_type       (A.copy_out_module_type       x)
  let copy_out_sig_item          x = B.copy_out_sig_item          (A.copy_out_sig_item          x)
  let copy_out_type_extension    x = B.copy_out_type_extension    (A.copy_out_type_extension    x)
  let copy_out_phrase            x = B.copy_out_phrase            (A.copy_out_phrase            x)
  let copy_mapper                x = B.copy_mapper                (A.copy_mapper                x)
  (*$*)
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
  (*$ foreach_type (fun _ s -> printf "let copy_%-21s = M.copy_%s\n" s s) *)

  let copy_structure             = M.copy_structure
  let copy_signature             = M.copy_signature
  let copy_toplevel_phrase       = M.copy_toplevel_phrase
  let copy_core_type             = M.copy_core_type
  let copy_expression            = M.copy_expression
  let copy_pattern               = M.copy_pattern
  let copy_case                  = M.copy_case
  let copy_type_declaration      = M.copy_type_declaration
  let copy_type_extension        = M.copy_type_extension
  let copy_extension_constructor = M.copy_extension_constructor
  let copy_out_value             = M.copy_out_value
  let copy_out_type              = M.copy_out_type
  let copy_out_class_type        = M.copy_out_class_type
  let copy_out_module_type       = M.copy_out_module_type
  let copy_out_sig_item          = M.copy_out_sig_item
  let copy_out_type_extension    = M.copy_out_type_extension
  let copy_out_phrase            = M.copy_out_phrase
  let copy_mapper                = M.copy_mapper
  (*$*)
end

(* KNOWN VERSIONS *)

(*$ foreach_version (fun suffix version ->
      printf "module OCaml_%s = struct\n" suffix;
      printf "  module Ast = Ast_%s\n" suffix;
      printf "  include Make_witness(Ast_%s)\n" suffix;
      printf "  let string_version = %S\n" version;
      printf "end\n"
    )
*)
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
(*$*)

let all_versions : (module OCaml_version) list = [
  (*$foreach_version (fun suffix _ ->
       printf "(module OCaml_%s : OCaml_version);\n" suffix)*)
  (module OCaml_402 : OCaml_version);
  (module OCaml_403 : OCaml_version);
  (module OCaml_404 : OCaml_version);
  (module OCaml_405 : OCaml_version);
  (*$*)
]

let chain = Chain ((module OCaml_402), lazy (invalid_arg "Unknown Ast"))
(*$foreach_version_pair (fun x y ->
    printf "let chain = Chain ((module OCaml_%s), " y;
    printf "lazy ((module Migrate_parsetree_%s_%s),\n" y x;
    printf "      (module Migrate_parsetree_%s_%s), chain))\n" x y
  )*)
let chain = Chain ((module OCaml_403), lazy ((module Migrate_parsetree_403_402),
                                             (module Migrate_parsetree_402_403), chain))
let chain = Chain ((module OCaml_404), lazy ((module Migrate_parsetree_404_403),
                                             (module Migrate_parsetree_403_404), chain))
let chain = Chain ((module OCaml_405), lazy ((module Migrate_parsetree_405_404),
                                             (module Migrate_parsetree_404_405), chain))
(*$*)

module Convert = Make_conversion(struct let latest = Latest chain end)

module OCaml_current = OCaml_(*#concat OCAML_VERSION*)

(* Make sure the preprocessing worked as expected *)
let _f (x : Parsetree.expression) : OCaml_current.Ast.Parsetree.expression = x
