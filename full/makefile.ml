module Pkg = Paths.Pkg
module Pth = Paths.S

type param =
  {
    all: bool;
    native: bool;
    bytecode: bool;
    abs_path: bool;
    slash:string;
    one_line: bool;
    implicits: bool;
    shared:bool;
    includes: string list;
  }

let preprocess_deps includes unit =
  let replace ({Deps.pkg; _ } as dep) l = match pkg with
    | { Pkg.source = Unknown; file = [name] } ->
      let pkg = Option.default pkg (Name.Map.find_opt name includes) in
      {dep with pkg} :: l
    | { Pkg.source = Pkg _ ; _ }  -> l
    | _ -> dep :: l
  in
  let deps = Deps.of_list @@ Deps.fold replace (Unit.deps unit) [] in
  Unit.update deps unit

let implicit_dep synonyms path =
  (* implicitely looks for interface/implementation files.
     TODO: allow separated pair of .ml/.mli
  *)
  let exists ext =  Sys.file_exists @@ Support.remove_extension
      (Pkg.filename path) ^ "." ^ ext in
  Name.Map.fold (fun ext (info:Common.info) (found:bool Unit.pair) ->
      match info.kind with
      | Interface when info.format <> Cmi ->
        { found with mli = found.mli || exists ext }
      | Implementation ->
        { found with ml = found.ml || exists ext }
      | _ -> found
    )
    synonyms {ml=false;mli=false}



let expand_includes policy synonyms includes =
  let read_dir expanded dir =
    let dir = Common.expand_dir dir in
    if Sys.file_exists dir && Sys.is_directory dir then
      let files = Sys.readdir dir in
      let dir = if dir = "." then [] else Paths.S.parse_filename dir in
      Array.fold_left (fun m x ->
          let policy =
            let open Fault in
            Policy.register ~lvl:Level.info
              Codept_policies.unknown_extension policy in
          match Common.classify policy synonyms x with
          | None | Some { Common.kind = Signature; _ } -> m
          | Some { Common.kind = Interface | Implementation ; _ } ->
            Name.Map.add (Read.name x)
              Pkg.( dir / local x) m
        )
        expanded files
    else
      expanded
  in
  List.fold_left read_dir Name.Map.empty includes


let tokenize_deps includes param input dep (unit,imore,dmore) =
  let unit = preprocess_deps includes unit in
  let make_abs = Common.make_abs param.abs_path in
  let pkg_pp = Pkg.pp_gen param.slash in
  let open Unit in
  let dep x= make_abs @@ dep x in
  let tok f x = Format.asprintf "%a" f x in
  let compare x y = compare (Pkg.module_name y) (Pkg.module_name x) in
  let tokens l = List.map (tok pkg_pp)
      (List.map make_abs l) in
  tok pkg_pp ( make_abs @@ input unit.src)
  :: tokens imore
  @ ":"
    ::  tokens
      (
        List.sort compare
        @@ List.rev_map dep @@ Unit.local_dependencies unit
      )
  @ tokens dmore

let render param ppf l =
  let rec render pos = function
    | [] -> Format.pp_print_newline ppf ()
    | a :: q ->
      let n = String.length a in
      let pos' =
        if n + pos > 75 && param.one_line then
          (Pp.fp ppf " \\\n   "; 2 + n)
        else
          n + pos in
      let pos =
        if pos>0 then (Pp.fp ppf " "; pos' + 1) else pos' in
      Format.pp_print_string ppf a;
      render pos q in
  render 0 l

let print_deps includes param input dep ppf more =
  tokenize_deps includes param input dep more
  |> render param ppf

let regroup {Unit.ml;mli} =
  Unit.Group.Map.of_list ( mli @ ml)

let cmi_or synonyms or_ path =
  match implicit_dep synonyms path with
  | exception Not_found -> or_ path
  | { Unit.ml = true; mli = true } | { ml = false; mli=false } -> or_ path
  | { mli = false; ml = true } -> or_ path
  | { mli = true; ml = false } -> Pkg.cmi path

let cmo_or_cmi synonyms path =
  match implicit_dep synonyms path with
  | { Unit.mli = true; ml = _ } -> Pkg.cmi path
  |  _ -> Pkg.cmo path

let collision_error policy = function
  | a :: _ as l ->
    Fault.raise policy Standard_faults.local_module_conflict
      (a.Unit.path, List.map (fun u -> u.Unit.src) l)
  | [] -> ()

let unit_main policy param synonyms printer g =
  let cmo_or_cmi = cmo_or_cmi synonyms and cmi_or = cmi_or synonyms in
  let open Unit in
  let all = param.all in
  let if_all l = if all then l else [] in
  let g, err = Unit.Group.flatten g in
  List.iter (collision_error policy) [err.ml; err.mli];
  match g with
  | { ml= Some impl ; mli = Some intf } ->
    let cmi = Pkg.cmi impl.src in
    if not param.native then
      printer Pkg.cmo cmo_or_cmi (impl, [], cmi :: if_all [impl.src] );
    if not param.bytecode then begin
      let files = impl, if_all [Pkg.o impl.src], cmi :: if_all [impl.src] in
      printer (Pkg.cmx) (cmi_or Pkg.cmx) files;
      if param.shared then
        printer (Pkg.cmxs) (cmi_or Pkg.cmxs) files
    end;
    printer Pkg.cmi (Pkg.mk_dep all param.native) (intf,[], [])
  | { ml = Some impl; mli = None } ->
    begin
      let implicit = implicit_dep synonyms impl.src in
      let cmi = Pkg.cmi impl.src in
      let cmi_dep, cmi_adep =
        if param.implicits && implicit.mli then [cmi], [] else [], [cmi] in
      if not param.native then
        begin
          printer Pkg.cmo cmo_or_cmi
            (impl, if_all cmi_adep, if_all [impl.src] @ cmi_dep)
        end;
      if not param.bytecode then
        printer Pkg.cmx (cmi_or Pkg.cmx)
          (impl, if_all (Pkg.o impl.src :: cmi_adep),
           if_all [impl.src] @ cmi_dep )
    end
  | { ml = None; mli = Some intf } ->
    printer Pkg.cmi (Pkg.mk_dep all param.native) (intf,[],[])
  | { ml = None; mli = None } -> ()


let main policy ppf synonyms param units =
  let includes = expand_includes policy synonyms param.includes in
  let print_deps x y = print_deps includes param x y ppf in
  let m =regroup units in
  Unit.Group.Map.iter (unit_main policy param synonyms print_deps) m
