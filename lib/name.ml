module Core = struct
  type t = string
  type name = t
  let compare (x:name) (y:name) = compare x y
  let pp = Pp.string
end
include Core

module Set = struct
  include Set.Make(Core)
  let pp ppf s = Pp.(clist Core.pp) ppf (elements s)
  let of_list = List.fold_left (fun set x -> add x set) empty
end
type set = Set.t

module Map =
struct
  include (Map.Make(Core))
  let find_opt k m = try Some(find k m) with Not_found -> None
  let union' s = union (fun _key _m1 m2 -> Some m2) s
  let update key f m = try add key (f (find key m)) m with Not_found -> m
end

type 'a map = 'a Map.t
