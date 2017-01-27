(* A completely ad-hoc text substitution language friendly generating line
   directives *)

let delimit_command input offset =
  let len = String.length input in
  let i = ref offset in
  let depth = ref 1 in
  let in_string = ref false in
  while !i < len && !depth > 0 do
    begin if !in_string then
      match input.[!i] with
      | '"' -> in_string := false;
      | '\\' -> incr i;
      | _ -> ()
    else
      match input.[!i] with
      | '"' -> in_string := true;
      | '(' -> incr depth
      | ')' -> decr depth
      | _ -> ()
    end;
    incr i
  done;
  if !in_string then failwith "Unterminated string";
  if !depth > 0 then failwith "Unterminated command";
  if input.[!i-2] <> '*' then failwith "Command not terminated with *)";
  !i

type command =
  | Atom of string
  | Int of int | String of string | Char of char | Float of float
  | List of command list

let parse =
  let lexer = Genlex.make_lexer ["("; ")"] in
  fun str ->
    let stream = lexer (Stream.of_string str) in
    (*Stream.iter (function
        | Genlex.Kwd x    -> Printf.eprintf "Kwd %S\n" x
        | Genlex.Char x   -> Printf.eprintf "Char %C\n" x
        | Genlex.Int x    -> Printf.eprintf "Int %d\n" x
        | Genlex.Float x  -> Printf.eprintf "Float %f\n" x
        | Genlex.String x -> Printf.eprintf "String %S\n" x
        | Genlex.Ident x  -> Printf.eprintf "Ident %S\n" x
      )
      stream;*)
    let rec loop first acc =
      match Stream.next stream with
      | Genlex.Kwd "(" ->
          let sub = loop false [] in
          loop first (List sub :: acc)
      | Genlex.Kwd ")" when first -> failwith "Unclosed '(*'"
      | Genlex.Kwd ")" -> List.rev acc
      | Genlex.Char x   -> loop first (Char x :: acc)
      | Genlex.Int x    -> loop first (Int x :: acc)
      | Genlex.Float x  -> loop first (Float x :: acc)
      | Genlex.String x -> loop first (String x :: acc)
      | Genlex.Ident x  -> loop first (Atom x :: acc)
      | Genlex.Kwd _ -> assert false
      | exception Stream.Failure when first -> List.rev acc
      | exception Stream.Failure -> failwith "Unclosed '('"
    in
    List (loop true [])

let concat_map f l =
  List.concat (List.map f l)

let variables : (string, string list) Hashtbl.t = Hashtbl.create 7

let rec exec : command list -> string list = function
  | Atom "foreach" :: Atom name :: range :: actions ->
      let range = eval range in
      let first = "first_" ^ name in
      Hashtbl.add variables first ["first"];
      let result = concat_map (fun value ->
          Hashtbl.add variables name [value];
          let result = concat_map eval actions in
          Hashtbl.replace variables first [];
          Hashtbl.remove variables name;
          result
        ) range
      in
      Hashtbl.remove variables first;
      result
  | Atom "define" :: Atom name :: params ->
      Hashtbl.replace variables name (concat_map eval params);
      []
  | [Atom "if"; a; b] ->
      begin match eval a with
      | _ :: _ -> eval b
      | [] -> []
      end
  | [Atom "if"; a; b; c] ->
      begin match eval a with
      | _ :: _ -> eval b
      | [] -> eval c
      end
  | [Atom "defined"; Atom var] ->
      begin match Hashtbl.find variables var with
      | exception Not_found -> []
      | [] -> [""]
      | x -> x
      end
  | Atom "ifdef" :: var :: tail ->
      exec (Atom "if" :: List [Atom "defined"; var] :: tail)

  | Atom "concat" :: params ->
      concat_map eval params
  | _ -> failwith "invalid command"

and eval = function
  | Int i -> [string_of_int i]
  | String s -> [s]
  | Float f -> [string_of_float f]
  | Char c -> [String.make 1 c]
  | Atom var ->
      (try Hashtbl.find variables var
       with Not_found -> failwith ("Unbound variable " ^ var))
  | List args -> exec args

let process oc fname input =
  let len = String.length input in
  let line = ref 1 and bol = ref 0 and i = ref 0 in
  let sync_line () =
    Printf.fprintf oc "# %d %S\n%s"
      !line fname (String.make (!i - !bol) ' ')
  in
  sync_line ();
  while !i < len do
    let c = input.[!i] in
    incr i;
    match c with
    | '\n' ->
        incr line; bol := !i;
        output_char oc '\n'
    | '(' when !i + 1 < len && input.[!i] = '*' && input.[!i+1] = '$' ->
        let offset = !i+2 in
        let i' = delimit_command input offset in
        let command = String.sub input offset (i' - offset - 2) in
        let result = String.concat "" (eval (parse command)) in
        output_string oc result;
        let should_sync = ref (
            try ignore (String.index result '\n'); true
            with Not_found -> false
          ) in
        for j = !i to i' - 1 do
          if input.[j] = '\n' then (
            incr line;
            bol := j+1;
            should_sync := true;
          )
        done;
        i := i';
        if !should_sync then
          (output_char oc '\n'; sync_line ());
    | c -> output_char oc c
  done

let usage msg =
  Printf.eprintf "Usage: %s [-Dvar=value] [-Dvar] input\n" Sys.executable_name;
  if msg <> "" then Printf.eprintf "%s\n" msg;
  exit 1

let () =
  let input = ref [] in
  for i = Array.length Sys.argv - 1 downto 1 do
    let s = Sys.argv.(i) in
    let len = String.length s in
    if len  > 2 && s.[0] = '-' && s.[1] = 'D' then (
      let k, v =
        match String.index s '=' with
        | exception Not_found -> String.sub s 2 (len - 2), ""
        | n -> String.sub s 2 (n - 2), String.sub s (n + 1) (len - n - 1)
      in
      let vs = try Hashtbl.find variables k with Not_found -> [] in
      Hashtbl.replace variables k (v :: vs)
    ) else (
      input := s :: !input
    )
  done;
  match !input with
  | [filename] ->
      let ic = open_in filename in
      let sz = in_channel_length ic in
      let input = really_input_string ic sz in
      close_in ic;
      process stdout filename input
  | [] -> usage "No input file"
  | filenames ->
      usage ("Only one input allowed (received " ^ String.concat ", " filenames)

