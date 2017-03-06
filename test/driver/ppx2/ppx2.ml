(* Rewrite [%fourty_two] as 42 *)

open Migrate_parsetree
open OCaml_403.Ast
open Parsetree

let cmd_line_arg = ref "unset"

let rewriter _config _cookies _args =
  let super = Ast_mapper.default_mapper in
  let expr self e =
    match e.pexp_desc with
    | Pexp_extension ({ txt = "cmd_line_arg"; _ }, PStr []) ->
      { e with pexp_desc = Pexp_constant (Pconst_string (!cmd_line_arg, None)) }
    | _ -> super.expr self e
  in
  { super with expr }

let () =
  Migrate_driver.register ~name:"ppx2"
    ~args:[("-message", Arg.Set_string cmd_line_arg, "")]
    (module OCaml_403)
    rewriter
