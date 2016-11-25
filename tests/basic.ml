
module M = struct module A = struct end end
module A = struct
  open M
  open Ext
  open A
end     
module B = struct
  open Ext2
  open A
end
