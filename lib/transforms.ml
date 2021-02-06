
let debug fmt = Format.ifprintf Pp.err ("Debug:" ^^ fmt ^^"@.")

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
      | Module.Alias _ | Namespace _  | Link _ | Abstract _
      | Fun _ as x -> x
      | Sig m ->
        Module.Sig { m with signature = remove_path_from rest m.signature }
    in
    { defs with modules= Name.Map.update a update defs.modules }

let with_deletions dels d =
  Paths.S.Set.fold remove_path_from dels d


let filename loc = fst loc

type level = Module.level = Module | Module_type


module F = Standard_faults

let open_diverge_module policy loc x =
  let open Module.Partial in
  match x.mty with
  | Abstract _ | Fun _ -> Summary.empty
  | Sig ({ signature=Blank; _ } |{ origin = Phantom _; _ } as r) ->
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
  | Sig { signature=(Divergence _ | Exact _ as s); _}  -> Summary.View.see s

let open_diverge pol loc x = match x.kind with
  | Mty Sig m -> open_diverge_module pol loc (Module.Partial.of_module x.name m)

  (* FIXME: type error *)
  | Namespace modules ->
    Summary.View.see @@ Module.Exact { Module.Def.empty with modules }
  | Mty (Abstract _ | Fun _ ) -> Summary.empty

let open_ pol loc x = open_diverge_module pol loc x

let of_partial policy loc p =
  match Summary.of_partial p with
  | Error def -> Fault.raise policy F.structure_expected (loc,p); def
  | Ok def -> def

let gen_include policy loc x =
  match x.Module.Partial.mty with
  | Abstract _ | Fun _ ->  (* TODO ERROR *) Summary.empty
  | Sig s ->
    if s.signature = Blank && s.origin = First_class
    then Fault.raise policy F.included_first_class loc;
    of_partial policy loc x

let bind_summary level name expr =
  let m = Module.Partial.to_module ~origin:Submodule expr in
  Summary.define ~level [name,m]

let apply_arg policy loc ~f:(p:Module.Partial.t) ~arg =
  match p.mty with
  | Fun (Some {Module.Arg.name=Some _arg_name; signature=param } , body) ->
    debug "Applying %a to %a @." Module.Partial.pp p Module.Partial.pp arg;
    let mty = let open Module.Partial in
      of_extended_mty @@ apply ~arg:(extend arg.mty) ~param:(extend param) ~body:(extend body)
    in
    { p with mty }
  | Fun (_, r) -> { p with mty = r }
  | Sig _ | Abstract _  ->
    if Module.Partial.is_exact p then
      (* we guessed the arg wrong *)
      Fault.raise policy F.applied_structure (loc,p);
    p


type 'a query_result =
  { main:'a; deps: Deps.t; msgs: Fault.t list }
