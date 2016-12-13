open open
module A = Ext
type t = B | C | D
include Ext2
let x = Ext3.y
let w = Ext4.x.Ext5.field
