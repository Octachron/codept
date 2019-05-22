module rec M: sig type t end = struct
  open N
  type nonrec t = t
end
and N: sig type t include S end = struct
  type t
end


module X: sig type t end = struct
  open Ext
  type t
end


module rec Y: sig type t end = struct
  open X
  type nonrec t = t
end


module rec X: sig module Inner:sig end end = struct
  open Y
  open Inner
  open Xeno2
end
and Y:sig open Xeno module Inner:sig end end = struct
  open X
  open Inner
  open Xeno3
end
