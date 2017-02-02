module M = struct module N = struct end end

open Extern
module A = M.N
open A
