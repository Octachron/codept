
let dp1 f x ppf = Pp.fp ppf f x
let dp4 f x y z w ppf  = Pp.fp ppf f x y z w

let mk ?(special="stdlib") ?(nms=["Stdlib"])  ?(mds=[]) ?(mts=[]) name =
  let path = Namespaced.make ~nms name in
  let origin =
    Module.Origin.Unit {source={source=Special special; file=path}; path} in
  Module.Sig { origin; signature=Module.Sig.of_lists mds mts}


let compare v =
  let computed =
    match Bundle.versioned_stdlib v with
    | ["Stdlib", Namespace n] ->
      n
    | _ -> assert false
  in
  let ref =
    (* we add Stdlib.LargeFile by hand *)
    let largeFile = "LargeFile" in
    Name.Map.add largeFile (mk largeFile)
    @@ Bundle_refs.Stdlib_414.modules in
  let diff k x y = match x, y with
    | Some _, None -> Some (Error (dp1 "Missing %s" k))
    | None, Some _ -> Some (Error (dp1 "Wrong additional module %s" k))
    | None, None -> None
    | Some x, Some y ->
      if Module.Equal.eq x y then
        Some (Ok k)
      else
        Some(
          Error
            ( dp4 "@[<v>ref:@,%a@,computed:@,%a@]@."
               Module.pp x
               Module.pp y
            )
        )
  in
  Module.Equal.dict computed ref ||
  begin
    let diff = Name.Map.merge diff ref computed in
    Name.Map.iter (fun k r -> match r with
        | Ok k -> Pp.e "@[<v>%s[%a]:@,@]" k Format_tags.(tagged Em) "ok"
        | Error s -> Pp.e "@[<v>%s[%a]:@;%t@;@]" k Format_tags.(tagged Error) "error"  s
      ) diff;
    false
  end


let () =
  Format_tags.enable Pp.err;
  assert (compare (4, 14))
