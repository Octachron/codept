module M = Module
module S = Module.Sig
module Def = Definition.Def

module Envt = struct
  type t = M.signature

  let empty = S.empty

  let proj lvl env = match lvl with
    | M.Module -> env.M.modules
    | M.Module_type -> env.module_types

  let root_origin level path env =
    let a, lvl = match path with
      | []-> raise @@ Invalid_argument "Compute.Envt.root_origin: empty path"
      | [a] -> a, level
      | a :: _ -> a, M.Module in
    let m = Name.Map.find a @@ proj lvl env in
    m.origin

  let rec find ~transparent ?alias level path env =
    match path with
    | [] -> raise (Invalid_argument "Envt.find cannot find empty path")
    | [a] -> Name.Map.find a @@ proj level env
    | a :: q ->
      let m = Name.Map.find a env.modules in
      find ~transparent ?alias level q m.signature

  let find_partial ~transparent level path env =
    let m = find ~transparent ~alias:false level path env in
    m.signature

  let (>>) = Def.(+@)
  let add_module = S.add


end


module Tracing = struct

  module P = Package.Path
  type core = { env: Envt.t; protected: P.t Name.map }
  type t = { env: Envt.t;
             protected: P.t Name.map;
             deps: P.set ref;
             cmi_deps: P.set ref
           }


  let resolve n env =
   try Name.Map.find n env.protected with
     | Not_found -> P.{ package = Unknown; file = [n] }

  let record n env =
    env.deps := P.Set.add (resolve n env) !(env.deps)
  let record_cmi n env = env.cmi_deps := P.Set.add (resolve n env) !(env.cmi_deps)


  let start protected = { protected; env = Envt.empty }

  let empty ()= {deps = ref P.Set.empty; protected = Name.Map.empty;
               env = Envt.empty; cmi_deps = ref P.Set.empty }

  let create ({ protected; env }:core) :t =
    { (empty ())  with env; protected }

  let deps env = env.deps

  let name path = List.hd @@ List.rev path

  let alias_chain start env a root = function
    | Module.Alias n when start-> Some n
    | Alias n -> Option.( root >>| fun r -> record_cmi r env; n )
    | Unit when start -> Some a
    | _ -> None

  let guard transparent f x = if transparent then f x else None

  let prefix level a env =
    match Name.Map.find a @@ Envt.proj level env.env with
    | exception Not_found -> a
    | { origin = Alias n; _ } -> n
    | _ -> a

  let rec find0 ~transparent start root level path env =
    match path with
    | [] -> raise (Invalid_argument "Envt.find cannot find empty path")
    | [a] ->
      let m = Name.Map.find a @@ Envt.proj level env.env in
      let root = guard transparent (alias_chain start env a root) m.origin in
      root, m
    | a :: q ->
      let m = Name.Map.find a env.env.modules in
      let root = guard transparent (alias_chain start env a root) m.origin in
      find0 ~transparent false root level q { env with env = m.signature }

  let check_alias name env =
    match (Name.Map.find name env.env.modules).origin with
    | Unit -> true
    | exception Not_found -> true
    | _ -> false

  let find ~transparent ?(alias=false) level path env =
    match find0 ~transparent true None level path env with
    | Some root, x when not transparent || not alias
      ->
      ( if check_alias root env then record root env; x)
    | Some _, x -> x
    | None, x ->
      let root = prefix level (List.hd path) env in
      (if check_alias root env then record root env; x)
    | exception Not_found ->
      let root = prefix level (List.hd path) env in
      if Name.Map.mem root env.protected then
        raise Not_found
      else begin
        if not alias then record root env;
        Module.create ~origin:M.Extern (name path) S.empty
      end
  (* | Not_found -> raise Not_found *)

  let find_partial ~transparent level path core =
    let m = find ~transparent ~alias:false level path core in
    let env = m.signature in
    { (empty()) with env }

  let (>>) e1 sg = { e1 with env = Envt.( e1.env >> sg) }

  let add_module e m = { e with env = S.add e.env m }
  let add_core (c:core) m = { c with env = S.add c.env m }
end

module Interpreters = struct
  module Sg = Interpreter.Make(Envt)
  module Tr = Interpreter.Make(Tracing)
end
