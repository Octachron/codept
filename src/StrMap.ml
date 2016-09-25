type name = string

include (Map.Make(struct type t = name let compare = compare end))
let find_opt k m = try Some(find k m) with Not_found -> None
