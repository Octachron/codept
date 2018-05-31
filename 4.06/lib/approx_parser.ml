
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
  let minor x = M2l.Minor x in
  match opt_name with
  | None -> l
  | Some name ->
    let open M2l in
    match l with
    | { Loc.data = Minor m; loc } :: q ->
      let m = Annot.merge (Annot.access name) {data=m;loc} in
      Loc.fmap minor m :: q
    | l -> Loc.fmap minor (Annot.access name) :: l

let stack = ref []

let token lexbuf =
  match !stack with
  | [] -> Lexer.token lexbuf
  | a :: q -> stack := q; a

let rewind a = stack := a :: !stack
let locate lexbuf =
  let open Lexing in
  let ext pos = pos.pos_lnum, pos.pos_cnum - pos.pos_bol in
  Loc.compress @@
  Multiline { start = ext lexbuf.lex_start_p; stop = ext lexbuf.lex_curr_p }

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
  | Parser.UIDENT name ->
    let loc = locate lexbuf in
    begin match inf_bind lexbuf with
      | None ->
        M2l.(Loc.create loc @@ Bind { name; expr = Str []}) :: inf_start lexbuf
    | Some alias ->
      M2l.(
        Loc.create (Loc.merge loc alias.Loc.loc) @@
        Bind { name; expr = Ident alias.data}) :: inf_start lexbuf
    end
  | Parser.EOF -> []
  | _ -> inf_start lexbuf
and inf_bind lexbuf =
  let module L = Loc in
  match token lexbuf with
  | Parser.EQUAL ->
    begin match token lexbuf with
      | UIDENT name ->
        let loc = locate lexbuf in
        let rest: _ L.ext = !inf_path_at_dot lexbuf in
        Some { data= name :: rest.data; loc = L.merge loc rest.loc }
      | _ -> None
    end
  | _ -> None

and inf_open lexbuf =
  match token lexbuf with
  | Parser.UIDENT name ->
    let loc = locate lexbuf in
    let rest = !inf_path_at_dot lexbuf in
    let loc = Loc.merge loc rest.loc in
    M2l.{ data = Open ( name :: rest.data ); loc }  :: inf_start lexbuf
  | Parser.EOF -> []
  | _ -> inf_start lexbuf
and inf_include lexbuf =
  match token lexbuf with
  | Parser.UIDENT name ->
    let loc = locate lexbuf in
    let rest = !inf_path_at_dot lexbuf in
    let loc = Loc.merge loc rest.loc in
    let path = name :: rest.data in
    M2l.{ data = Include (Ident path); loc }  :: inf_start lexbuf
  | Parser.EOF -> []
  | _ -> inf_start lexbuf
and inf_uident name lexbuf =
  match token lexbuf with
  | Parser.DOT ->
    let loc = locate lexbuf in
    let _ = !inf_path lexbuf in
    Some { data = [name]; loc }
  | x -> rewind x; None
  | exception Lexer.Error _ -> None
and inf_path lexbuf =
  match token lexbuf with
  | Parser.UIDENT name ->
    let loc0 = locate lexbuf in
    let {Loc.loc; data} = !inf_path_at_dot lexbuf in
    { Loc.data = name :: data; loc = Loc.merge loc0 loc }
  | _ -> Loc.nowhere []
and inf_path_at_dot lexbuf =
  match token lexbuf with
  | Parser.DOT -> inf_path lexbuf
  | a -> rewind a;  Loc.nowhere []
and (~~) f x = try f x with Lexer.Error _ -> inf_start x
and (!) f x = try f x with Lexer.Error _ ->  Loc.nowhere []


let lower lex =
  let r = snd @@ M2l.Normalize.all @@ inf_start lex in
  stack := [];
  r

let to_upper_bound m2l =
  let union x y =
    Loc.{data = M2l.Annot.Access.merge x.data y.data; loc = merge x.loc y.loc } in
  let add x s  =
    Loc.{ data =
            Paths.S.Map.add x.data (x.loc, Deps.Edge.Normal) s.data;
          loc = merge x.loc s.loc } in
  let open M2l in
  let open Loc in
  let access =
    List.fold_left (fun s elt ->
        let locate x = Loc.create elt.loc x in
        match elt.data with
        | Minor { access; _ } -> union (locate access) s
        | Open path -> add (locate path) s
        | Bind {expr = Ident path; _}
        | Include (Ident path) -> add (locate path) s
        | _ -> s
      ) (Loc.nowhere M2l.Annot.Access.empty) m2l in
  [Loc.fmap (fun access -> Minor { Annot.empty.data with access }) access]

let lower_bound filename =
  let chan = open_in filename in
  let lex = Lexing.from_channel chan in
  let r = lower lex in
  let () = close_in chan in
  r

let file filename =
  let name = Read.name filename in
  let chan = open_in filename in
  let lex = Lexing.from_channel chan in
  let low = lower lex in
  let () = close_in chan in
  name, low, to_upper_bound low
