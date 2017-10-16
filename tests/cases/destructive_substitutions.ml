module type s = sig module Ext: sig end module Inner:sig end end
module Void = struct end

module F(): s with module Ext := Void = struct module Inner = struct end end

module M = F()
open M
open Inner
open Ext
