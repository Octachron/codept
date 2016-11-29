
(* Approximate parser.
   Heuristic: try to detect
   - open Module_path
   - include Module_path
   - Module_path . not module identifier
   - module M
   and match them to the corresponding toplevel structure item.

   As a consequence, the set of bound modules at any point in the approximated
   m2l ast should be a superset of the set of bound module in the wished AST.
   Notwithstanding inclusion of UIDENT signature, the dependencies inferred
   for the corresponding AST shall yield a lower bound for the wished dependencies.

   We can also transform the corresponding approximated AST to eliminate all
   binding and convert opening and including modules to module access. Infering
   dependencies from this AST will gives us an upper bound of wished dependencies.
*)


let (@%) opt_name l =
  match opt_name with
  | None -> l
  | Some name ->
    let open M2l in
    match l with
    | Minor m :: q ->
      Minor { m with access = Name.Set.add name m.access} :: q
    | l -> Minor (Annot.access name) :: l


let token = Lexer.token
let rec inf_start lexbuf =
  match token lexbuf with
  | Parser.OPEN -> ~~inf_open lexbuf
  | Parser.INCLUDE -> ~~inf_include lexbuf
  | Parser.MODULE -> ~~inf_module lexbuf
  | Parser.UIDENT name ->
    let access =  inf_uident name lexbuf in
    access @% inf_start lexbuf
  | Parser.EOF -> []
  | _ -> inf_start lexbuf
  | exception Lexer.Error _ -> inf_start lexbuf
and inf_module lexbuf =
  match token lexbuf with
  | Parser.UIDENT name -> M2l.(Bind { name; expr = Str []}) :: inf_start lexbuf
  | Parser.EOF -> []
  | _ -> inf_start lexbuf
and inf_open lexbuf =
  match token lexbuf with
  | Parser.UIDENT name ->
    M2l.Open ( name :: !inf_path_at_dot lexbuf)  :: inf_start lexbuf
  | Parser.EOF -> []
  | _ -> inf_start lexbuf
and inf_include lexbuf =
  match token lexbuf with
  | Parser.UIDENT name ->
    M2l.(Include (Ident (name :: !inf_path_at_dot lexbuf)))  :: inf_start lexbuf
  | Parser.EOF -> []
  | _ -> inf_start lexbuf
and inf_uident name lexbuf =
  match token lexbuf with
  | Parser.DOT ->
    let _ = !inf_path lexbuf in
    Some name
  | _ -> None
  | exception Lexer.Error _ -> None
and inf_path lexbuf =
  match token lexbuf with
  | Parser.UIDENT name -> name :: !inf_path_at_dot lexbuf
  | _ -> []
and inf_path_at_dot lexbuf =
  match token lexbuf with
  | Parser.DOT -> inf_path lexbuf
  | _ -> []
and (~~) f x = try f x with Lexer.Error _ -> inf_start x
and (!) f x = try f x with Lexer.Error _ -> []


let lower lex = snd @@ M2l.Normalize.all @@ inf_start lex

let to_upper m2l =
  let add, union = Name.Set.(add,union) in
  let open M2l in
  let access =
  List.fold_left (fun s elt -> match elt with
       | Minor { access; _ } -> union access s
       | Open path -> add (List.hd path) s
       | Include (Ident path) -> add (List.hd path) s
       | _ -> s
      ) Name.Set.empty m2l in
  [Minor { Annot.empty with access }]

let file filename =
  let name = Read.name filename in
  let chan = open_in filename in
  let lex = Lexing.from_channel chan in
  let low = lower lex in
  name, low, to_upper low
