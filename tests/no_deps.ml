module M = struct end
open M
module N = struct
        module A = struct end
end
include N.A
