module F(X:sig end) = struct module type Y = sig end end
module X = struct end
module Y: sig module M:F(X).Y end = struct module M = struct end end

module W : Ext.F(Ext2.X).M.s = struct end

module A = struct
        module F(X:sig end) = struct
        module M = struct
                module type s
        end
        end
end
module B = struct module X = struct end end

module C : A.F(B.X).M.s = struct end
