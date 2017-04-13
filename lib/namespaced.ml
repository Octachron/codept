type p = Paths.S.t
type t = { namespace: p; name: Name.t }
type namespaced = t

let pp ppf n = Pp.fp ppf "%a.%s" Paths.S.pp n.namespace n.name
let make ?(nms=[]) name = { namespace = nms; name }
let flatten n = n.namespace @ [n.name]

module Ordered = struct
  type nonrec t = t
  let compare: t -> t -> int = compare
end

module Map= struct
  include Map.Make(Ordered)
  let find_opt x m = try Some(find x m) with Not_found -> None
end
module Set = Set.Make(Ordered)
