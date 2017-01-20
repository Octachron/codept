module Pkg = Paths.Pkg
module Pth = Paths.S

type param =
  {
    all: bool;
    native: bool;
    bytecode: bool;
    abs_path: bool;
    slash:string;
    implicits: bool;
  }

let replace_deps includes unit =
  let replace = function
    | { Pkg.source = Unknown; file = [name] } as x ->
      begin
        try Name.Map.find name includes with Not_found -> x
      end
    | x -> x in
  { unit with Unit.dependencies =
                Pkg.Set.of_list
                @@ List.map replace
                @@ Pkg.Set.elements unit.Unit.dependencies }

let implicit_dep synonyms path =
  (* implicitely looks for interface/implementation files.
     TODO: allow separated pair of .ml/.mli
  *)
  let exists ext =  Sys.file_exists @@ Filename.remove_extension
      (Pkg.filename path) ^ "." ^ ext in
  Name.Map.fold (fun ext (info:Common.info) (found:bool Unit.pair) ->
      match info.kind with
      | Interface ->
        { found with mli = found.mli || exists ext }
      | Implementation ->
        { found with ml = found.ml || exists ext }
      | _ -> found
    )
        synonyms {ml=false;mli=false}

let print_deps (univ:Common.param) param order input dep ppf (unit,imore,dmore) =
  let unit = replace_deps univ.includes unit in
  let make_abs = Common.make_abs param.abs_path in
  let pkg_pp = Pkg.pp_gen param.slash in
  let sort = Sorting.toposort order Paths.Pkg.module_name in
  let open Unit in
  let dep x= make_abs @@ dep x in
  let ppl ppf l = Pp.(list ~sep:(s" ") ~post:(s" ") pkg_pp) ppf
      (List.map make_abs l) in
  Pp.fp ppf "%a %a:%a %a\n"
    pkg_pp ( make_abs @@ input unit.path)
    ppl imore
    Pp.(list ~pre:(s " ") ~sep:(s " ") pkg_pp)
    ( List.rev_map dep
      @@ sort
      @@ Common.local_dependencies sort unit
    )
    ppl dmore

let regroup {Unit.ml;mli} =
  let add l m = List.fold_left (fun x y -> Unit.Groups.R.Map.add y x) m l in
  add mli @@ add ml @@ Pth.Map.empty

let main ppf common_p param units =
  let all = param.all in
  let if_all l = if all then l else [] in
  let print_deps = print_deps common_p param in
  let order = Sorting.remember_order units.Unit.mli in
  let m =regroup units in
  let cmi_or or_ path =
    let open Unit in
    match implicit_dep common_p.synonyms path with
    | exception Not_found -> or_ path
    | { ml = true; mli = true } | { ml = false; mli=false } ->
        or_ path
    | { mli = false; ml = true } ->
      or_ path
    | { mli = true; ml = false } ->
      Pkg.cmi path in
  Pth.Map.iter (fun _k g ->
      let open Unit in
      match g with
      | { ml= Some impl ; mli = Some intf } ->
        let cmi = Pkg.cmi impl.path in
        if not param.native then
          print_deps order (Pkg.cmo) (cmi_or Pkg.cmo) ppf
            (impl, [], [cmi] @ if_all [impl.path] );
        if not param.bytecode then
          print_deps order (Pkg.cmx) (cmi_or Pkg.cmx) ppf
            (impl, if_all [Pkg.o impl.path], [cmi] @ if_all [impl.path] );
        print_deps order Pkg.cmi (Pkg.mk_dep all param.native)  ppf
          (intf,[], [] )
      | { ml = Some impl; mli = None } ->
        begin
          let implicit = implicit_dep common_p.synonyms impl.path in
          let cmi = Pkg.cmi impl.path in
          let imli =  param.implicits
                      && implicit.mli in
          let cmi_dep, cmi_adep =
            ( if imli then
                [cmi], []
              else [], [cmi] ) in
          if not param.native then
            begin
              print_deps order Pkg.cmo (cmi_or Pkg.cmo) ppf
                (impl, if_all cmi_adep, if_all [impl.path] @ cmi_dep)
            end;
          if not param.bytecode then
            print_deps order Pkg.cmx (cmi_or Pkg.cmx) ppf
              (impl,
               if_all ([Pkg.o impl.path] @ cmi_adep),
               if_all [impl.path] @ cmi_dep )
        end
      | { ml = None; mli = Some intf } ->
        print_deps order Pkg.cmi (Pkg.mk_dep all param.native) ppf
          (intf,[],[])
      | { ml = None; mli = None } -> ()
    ) m
