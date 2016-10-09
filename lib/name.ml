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
end
type set = Set.t

module Map =
struct
  include (Map.Make(Core))
  let find_opt k m = try Some(find k m) with Not_found -> None
  let union' s = union (fun _key _m1 m2 -> Some m2) s
end

type 'a map = 'a Map.t
