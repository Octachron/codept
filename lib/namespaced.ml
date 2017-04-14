type p = Paths.S.t
type t = { namespace: p; name: Name.t }
type namespaced = t


let pp ppf n = Pp.fp ppf "%a.%s" Paths.S.pp n.namespace n.name
let make ?(nms=[]) name = { namespace = nms; name }
let flatten n = n.namespace @ [n.name]
let of_path l =
  let rec split l = function
    | [a] -> l, a
    | a :: q -> split (a::l) q
    | [] -> raise @@ Invalid_argument("Namespaced.of_path: empty path")
  in
  let p, name = split [] l in
  { namespace = List.rev p; name }

let of_filename n =
  { namespace = n.namespace;
    name = Paths.S.( module_name @@  parse_filename n.name )
  }


module Ordered = struct
  type nonrec t = t
  let compare: t -> t -> int = compare
end

module Map= struct
  include Map.Make(Ordered)
  let find_opt x m = try Some(find x m) with Not_found -> None
end
module Set = Set.Make(Ordered)
