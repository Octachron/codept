module A = struct module B = struct end end

open E

module C = struct module D = struct end end

open A

open C

open D
