
let debug fmt = Format.ifprintf Pp.err ("Debug:" ^^ fmt ^^"@.")


type level = Module.level = Module | Module_type
module F = Standard_faults


type param = {
  policy: Fault.Policy.t;
  epsilon_dependencies: bool;
  transparent_extension_nodes: bool;
  transparent_aliases: bool
}


type answer_type =
  | Namespace of Module.dict
  | Mty of Module.sty
type answer = { name: Name.t; kind: answer_type }

let pp_answer ppf a =
  let pp_core ppf = function
    | Namespace dict -> Pp.fp ppf "Namespace (%a)" Module.pp_mdict dict
    | Mty x -> Module.pp ppf (Module.Partial.extend x)
  in
  Pp.fp ppf "(%s:%a)" a.name pp_core a.kind

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
      | Module.Alias _ | Module.Namespace _  | Module.Link _ | Module.Abstract _
      | Module.Fun _ as x -> x
      | Module.Sig m ->
        Module.Sig { m with signature = remove_path_from rest m.signature }
    in
    { defs with modules= Name.Map.update a update defs.modules }

let with_deletions dels d =
  Paths.S.Set.fold remove_path_from dels d

let open_diverge_module policy loc x =
  let open Module.Partial in
  match x.mty with
  | Module.Abstract _ | Module.Fun _ -> Summary.empty
  | Module.Sig ({ signature=Blank; _ } |{ origin = Phantom _; _ } as r) ->
    let kind =
      match r.origin with
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
      r.signature
  | Module.Sig { signature=(Divergence _ | Exact _ as s); _}  -> Summary.View.see s

let open_diverge policy loc x = match x.kind with
  | Mty Module.Sig m ->
    open_diverge_module policy loc (Module.Partial.of_module x.name m)
  | Mty (Module.Abstract _ as mty) ->
    Fault.raise policy F.opened (loc,mty,`Abstract);
    Summary.empty
  | Mty (Module.Fun _ as mty) ->
    Fault.raise policy F.opened (loc,mty,`Functor);
    Summary.empty


  (* ???: type error? *)
  | Namespace modules ->
    Summary.View.see @@ Module.Exact { Module.Def.empty with modules }

let open_ pol loc x = open_diverge_module pol loc x

let gen_include policy loc seed lvl x =
  let mty = match lvl with
    | Module -> x.Module.Partial.mty
    | Module_type -> Module.Partial.refresh seed x.Module.Partial.mty
  in
  match mty with
  | Module.Abstract _  ->
    Fault.raise policy F.included (loc,mty,`Abstract);
    Summary.empty
  | Module.Fun _ ->
    Fault.raise policy F.included  (loc,mty,`Functor);
    Summary.empty
  | Module.Sig s ->
    if s.signature = Blank && s.origin = First_class
    then Fault.raise policy F.included_first_class loc;
    Summary.of_signature s.signature

let bind_summary level name expr =
  let m = Module.Partial.to_module ~origin:Submodule expr in
  Summary.define ~level [name,m]

let apply_arg policy loc ~f:(p:Module.Partial.t) ~arg =
  match p.mty with
  | Module.Fun (Some {Module.Arg.name=Some _arg_name; signature=param } , body) ->
    debug "@[<hv 2>Applying@ @[%a@]@ to@ @[%a@]@]@." Module.Partial.pp p Module.Partial.pp arg;
    let mty = let open Module.Partial in
      of_extended_mty @@ apply ~arg:(extend arg.mty) ~param:(extend param) ~body:(extend body)
    in
    { p with mty }
  | Module.Fun (_, r) -> { p with mty = r }
  | Module.Sig _ | Module.Abstract _  ->
    if Module.Partial.is_exact p then
      (* we guessed the arg wrong *)
      Fault.raise policy F.applied_structure (loc,p);
    p


type 'a query_result =
  { main:'a; deps: Deps.t; msgs: Fault.t list }
