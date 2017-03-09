open Migrate_parsetree_versions

(** {1 State a rewriter can access} *)

type config = {
  tool_name       : string;
  include_dirs    : string list;
  load_path       : string list;
  debug           : bool;
  for_package     : string option;
}

type cookies

val get_cookie
  : cookies
  -> string
  -> 'types ocaml_version -> 'types get_expression option

val set_cookie
  : cookies
  -> string
  -> 'types ocaml_version -> 'types get_expression
  -> unit

(** {1 Registering rewriters} *)

type 'types rewriter = config -> cookies -> 'types get_mapper

val register
  :  name:string
  -> ?reset_args:(unit -> unit) -> ?args:(Arg.key * Arg.spec * Arg.doc) list
  -> 'types ocaml_version -> 'types rewriter
  -> unit

(** {1 Running registered rewriters} *)

val run_as_ast_mapper : string list -> Ast_mapper.mapper

val run_as_ppx_rewriter : unit -> 'a

val run_main : unit -> 'a
