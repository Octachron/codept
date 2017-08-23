module A = struct module M = Ext module N = Ext2 end
module type s = sig end
module type r = s with type t = A.M.x and type s = Ext3.t
