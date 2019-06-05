module A = Ext
open A
type w = B | C | G
include A
open Ext2
include include include
[@@@broken]
