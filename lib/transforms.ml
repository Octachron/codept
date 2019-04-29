
type param = {
  policy: Fault.Policy.t;
  epsilon_dependencies: bool;
  transparent_extension_nodes: bool;
  transparent_aliases: bool
}


type answer =
  | M of Module.m
  | Namespace of Module.namespace_content

(* Remove deleted modules with `with A.B.C.D := …` *)
let rec remove_path_from path = function
  | Module.Blank -> Module.Blank
  | Divergence d ->
    Divergence { d with
                 before = remove_path_from path d.before;
                 after = remove_path_from_sig path d.after
               }
  | Exact defs -> Exact (remove_path_from_sig path defs)
and remove_path_from_sig path defs = match path with
  | [] -> defs
  | [a] -> { defs with modules = Name.Map.remove a defs.modules }
  | a :: rest ->
    let update = function
      | Module.Alias _ | Namespace _ as x -> x
      | M m ->
        Module.M { m with signature = remove_path_from rest m.signature }
    in
    { defs with modules= Name.Map.update a update defs.modules }

let with_deletions dels d =
  Paths.S.Set.fold remove_path_from dels d


let filename loc = fst loc

type level = Module.level = Module | Module_type


module F = Standard_faults

let open_diverge_module policy loc x =
  let open Module.Partial in
  match x.origin, x.result with
  | _, Blank | Phantom _, _ ->
    let kind =
      match x.origin with
      | First_class ->
        Fault.raise policy F.opened_first_class (loc,x.name);
        Module.Divergence.First_class_module
      | Unit _ -> Module.Divergence.External
      | Phantom (_,d) -> d.origin
      | Submodule | Arg | Namespace -> Module.Divergence.External in
    let point =
      { Module.Divergence.root = x.name; origin=kind; loc } in
    Summary.View.see @@ Module.Sig.merge
      (Divergence
         { before = Module.Sig.empty; point; after = Module.Def.empty}
      )
      x.result
  | _, Divergence _ | _, Exact _ -> Summary.View.see x.result

let open_diverge pol loc = function
  | M x -> open_diverge_module pol loc (Module.Partial.of_module x)
  | Namespace {modules;_} -> (* FIXME: type error *)
    Summary.View.see @@ Module.Exact { Module.Def.empty with modules }

let open_ pol loc x = open_diverge_module pol loc x

let of_partial policy loc p =
  match Summary.of_partial p with
  | Error def -> Fault.raise policy F.structure_expected (loc,p); def
  | Ok def -> def

let gen_include policy loc x =
  if Module.Partial.(
      x.result = Blank && x.origin = First_class
    ) then
    Fault.raise policy F.included_first_class loc;
  of_partial policy loc x

let bind_summary level name expr =
  let m = Module.M (Module.Partial.to_module ~origin:Submodule name expr) in
  Summary.define ~level [m]

let drop_arg policy loc (p:Module.Partial.t) = match p.args with
  | _ :: args -> { p with args }
  | [] ->
    if Module.Partial.is_exact p then
      (* we guessed the arg wrong *)
      Fault.raise policy F.applied_structure (loc,p);
    p


type 'a query_result =
  { main:'a; deps: Deps.t; msgs: Fault.t list }
