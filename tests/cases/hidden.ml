(* there is an extern module Ext somewhere *)

module Ext = struct
  module Alias = Ext
end

open Ext.Alias
