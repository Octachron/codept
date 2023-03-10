(** Compare the reference bundles computed
 with `codept -no-alias-deps -export stdlib Stdlib.[<path_to_stdlib>]` (with the
template folder removed and the the Stdlib and Std_exit module remove) and
 the one computed by the bundled stdlib *)


let dp1 f x ppf = Pp.fp ppf f x
let dp4 f x y z w ppf  = Pp.fp ppf f x y z w

let mk ?(special="stdlib") ?(nms=["Stdlib"])  ?(mds=[]) ?(mts=[]) name =
  let path = Namespaced.make ~nms name in
  let origin =
    Module.Origin.Unit {source={source=Special special; file=path}; path} in
  Module.Sig { origin; signature=Module.Sig.of_lists mds mts}

let submodule ?special:_ ?nms:_ ?(mds=[]) ?(mts=[]) name =
  name, Module.Sig { origin=Submodule; signature=Module.Sig.of_lists mds mts}


module V = struct type t = int * int let compare=compare end
module Vmap = Map.Make(V)

let refs =
  List.fold_left (fun m (mj,mn,lib) -> Vmap.add (mj,mn) lib m) Vmap.empty
    (let open Bundle_refs in
     [
       5,  0, Stdlib_500.modules;
       4, 14, Stdlib_414.modules;
       4, 13, Stdlib_413.modules;
       4, 12, Stdlib_412.modules;
       4, 11, Stdlib_411.modules;
       4, 10, Stdlib_410.modules;
       4, 09, Stdlib_409.modules;
       4, 08, Stdlib_408.modules;
       4, 07, Stdlib_407.modules;
       4, 06, Stdlib_406.modules;
     ]
    )

let compare v ref =
  let computed =
    match Bundle.versioned_stdlib v with
    | ["Stdlib", Module.Namespace n] ->
      n
    | _ -> assert false
  in
  let largeFile = "LargeFile" in
  let pervasives = "Pervasives" in
  (* we add Stdlib.LargeFile by hand *)
  let ref =
    Name.Map.add largeFile (mk largeFile) ref
  in
  let ref =
    if v <= (4, 7) then
      Name.Map.add pervasives (mk pervasives ~mds:[submodule largeFile]) ref
    else
      ref
  in
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
  assert (Vmap.for_all compare refs)
