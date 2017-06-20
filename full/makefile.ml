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
  let replace l = function
    | ({ Pkg.source = Unknown; file = [name] } as p , e ) ->
      begin
        (try Name.Map.find name includes with Not_found -> p)
      , e end :: l
    | { Pkg.source = Pkg _ ; _ }, _  -> l
    | x -> x :: l

  in
  { unit with Unit.dependencies =
                Deps.of_list
                @@ List.fold_left replace []
                @@ Pkg.Map.bindings unit.Unit.dependencies }

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
            Policy.set_err
              (Codept_policies.unknown_extension, Level.info)
              policy in
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
        @@ List.rev_map dep @@ Common.local_dependencies unit
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
  let add l m = List.fold_left (fun x y -> Unit.Groups.R.Map.add y x) m l in
  add mli @@ add ml @@ Pth.Map.empty

let main policy ppf synonyms param units =
  let includes = expand_includes policy synonyms param.includes in
  let all = param.all in
  let if_all l = if all then l else [] in
  let print_deps = print_deps includes param in
  let m =regroup units in
  let cmi_or or_ path =
    let open Unit in
    match implicit_dep synonyms path with
    | exception Not_found -> or_ path
    | { ml = true; mli = true } | { ml = false; mli=false } ->
        or_ path
    | { mli = false; ml = true } ->
      or_ path
    | { mli = true; ml = false } ->
      Pkg.cmi path in
  let cmo_or_cmi path =
    let open Unit in
    match implicit_dep synonyms path with
    | { mli = true; ml = _ } -> Pkg.cmi path
    |  _ -> Pkg.cmo path in
  Pth.Map.iter (fun _k g ->
      let open Unit in
      let g, err = Groups.R.flatten g in
      let log_error  = function
        | a :: _ as l ->
          Fault.handle
            policy Standard_faults.module_conflict a.path @@
          List.map (fun u -> u.src) l
        | [] -> ()
      in
      log_error err.ml; log_error err.mli;
      match g with
      | { ml= Some impl ; mli = Some intf } ->
        let cmi = Pkg.cmi impl.src in
        if not param.native then
          print_deps (Pkg.cmo) cmo_or_cmi ppf
            (impl, [], [cmi] @ if_all [impl.src] );
        if not param.bytecode then begin
          print_deps (Pkg.cmx) (cmi_or Pkg.cmx) ppf
            (impl, if_all [Pkg.o impl.src],
             [cmi] @ if_all [impl.src] );
            if param.shared then
              print_deps (Pkg.cmxs) (cmi_or Pkg.cmxs) ppf
                (impl, if_all [Pkg.o impl.src],
                 [cmi] @ if_all [impl.src] )
        end;
          print_deps Pkg.cmi (Pkg.mk_dep all param.native)  ppf
          (intf,[], [] )
      | { ml = Some impl; mli = None } ->
        begin
          let implicit = implicit_dep synonyms impl.src in
          let cmi = Pkg.cmi impl.src in
          let imli =  param.implicits
                      && implicit.mli in
          let cmi_dep, cmi_adep =
            ( if imli then
                [cmi], []
              else [], [cmi] ) in
          if not param.native then
            begin
              print_deps Pkg.cmo cmo_or_cmi ppf
                (impl, if_all cmi_adep, if_all [impl.src] @ cmi_dep)
            end;
          if not param.bytecode then
            print_deps Pkg.cmx (cmi_or Pkg.cmx) ppf
              (impl,
               if_all ([Pkg.o impl.src] @ cmi_adep),
               if_all [impl.src] @ cmi_dep )
        end
      | { ml = None; mli = Some intf } ->
        print_deps Pkg.cmi (Pkg.mk_dep all param.native) ppf
          (intf,[],[])
      | { ml = None; mli = None } -> ()
    ) m
