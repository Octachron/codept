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
