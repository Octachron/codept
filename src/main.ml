
let test = {|
 open A
 let x = B.x
 include F(D)
 open C
 module U = struct end
 open U
|}

let lex_test = Lexing.from_string test

let ast = Parse.implementation lex_test

module L = Longident

let rec from_lid  =
  let open Resolver.Path in
  function
    | L.Lident s -> A s
    | L.Ldot (lid,s) -> S(from_lid lid,s)
    | L.Lapply (f,x) -> F {fn=from_lid f; arg = from_lid x }

open Parsetree
let txt x= x.Location.txt

let rec structure env str =
  List.fold_left structure_item env str
and structure_item env item =
  match item.pstr_desc with
  | Pstr_eval (exp, _attrs) -> expr env exp
  (* ;; exp [@@_attrs ] *)
  | Pstr_value (_rec_flag, vals)
    (* let P1 = E1 and ... and Pn = EN       (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN   (flag = Recursive)
         *) ->
    List.fold_left value_binding env vals
  | Pstr_primitive _desc -> env
        (*  val x: T
            external x: T = "s1" ... "sn" *)
  | Pstr_type (_rec_flag, type_declarations)
    (* type t1 = ... and ... and tn = ... *) ->
    List.fold_left type_declaration env type_declarations
  | Pstr_typext a_type_extension  (* type t1 += ... *) ->
    type_extension env a_type_extension
  | Pstr_exception an_extension_constructor
        (* exception C of T
           exception C = M.X *)
    -> extension_constructor env an_extension_constructor
  | Pstr_module a_module_binding (* module X = ME *) ->
    module_binding env a_module_binding
  | Pstr_recmodule module_bindings (* module rec X1 = ME1 and ... and Xn = MEn *)
    -> recmodules env module_bindings
  | Pstr_modtype a_module_type_declaration (*module type s = .. *) ->
        module_type_declaration env a_module_type_declaration
  | Pstr_open open_desc (* open M *) ->
        do_open env open_desc
  | Pstr_class class_declarations  (* class c1 = ... and ... and cn = ... *)
    -> List.fold_left class_declaration env class_declarations
  | Pstr_class_type class_type_declarations
  (* class type ct1 = ... and ... and ctn = ... *)
    -> List.fold_left class_type_declaration env class_type_declarations
  | Pstr_include include_dec (* include M *) ->
        do_include env include_dec
  | Pstr_attribute _attribute (* [@@@id] *)
    -> env
  | Pstr_extension (_id, _payload) (* [%%id] *) ->
    env
and expr env exp = env
and pattern env pat = env
and type_declaration env tyd = env
and type_extension env ty_ext = env
and exception_ env exn = env
and do_open env { popen_lid; _ } =
  let path = from_lid @@ txt popen_lid in
  Resolver.open_ path env
and do_include env incl = env
and extension_constructor env extc = env
and recmodules env mbs = env
and module_type_declaration env mdec = env
and class_declaration env c = env
and class_type_declaration env ct = env
and module_expr env mexpr = Resolver.Module.(Sig M.empty)
and value_binding env vb =
  expr env vb.pvb_expr
and module_binding env { pmb_name; pmb_expr; _ } =
  let open Resolver in
  let sign = module_expr (enter_module env) pmb_expr in
  let md = {Module.signature = sign;
            name = txt pmb_name } in
  Resolver.bind env md


let print_env env =
  let open Resolver in
  env.Env.unresolved.Unresolved.map
  |> Format.printf "@[%a@]\n" Unresolved.pp

let () =
  let open Resolver in
  let env = Env.empty in
  env
  |> open_ (Path.A "A")
  |> access (Path.A "B")
  |> open_ (Path.F {fn = Path.A "C"; arg = Path.A "D" } )
  |> print_env

let () =
  structure Resolver.Env.empty ast
  |> print_env
