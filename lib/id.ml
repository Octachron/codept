module Path_like = Pkg
type path_like = Path_like.t
type seed = path_like * int ref
type core = path_like * int
type t = Id of core [@@unboxed]

let compare (x:t) (y:t) = compare x y

let sch =
  let raw = Schematic.pair Path_like.sch Int in
  Schematic.custom raw (fun (Id x) -> x) (fun x -> Id x)

let pp ppf (Id(pkg,id)) = Pp.fp ppf "%a$%d" Path_like.pp pkg id

let create_seed nms = nms, ref 0

let create (nms,r) =
  incr r;
  Id (nms, !r)
