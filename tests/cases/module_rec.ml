module rec A : sig type t = int val x: t end = struct type t = int let x =0 end and
B : sig type t = int end = struct type t = int end

module rec A : sig
  type t = Atom of int | Set of S.t
  val compare: t -> t -> int
end = struct
  type t = Atom of int | Set of S.t
  let compare = compare
end
and  S : Set.S with type elt = A.t = Set.Make(A)

module rec A : sig module Inner:sig end end =struct module Inner = struct end end
and B : sig end = struct open A open Inner end
