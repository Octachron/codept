#directory "../_build/src";;
#load_rec "m2l.cmo";;
open M2l

let a1 =
  { name = "A1"; alias_of = []; args = []; signature = empty_sig };;

let a2 =
  { name = "A2"; alias_of = []; args = []; signature = empty_sig };;

let asig = { empty_sig with modules = empty |+> a1 |+> a2 };;

let a: module_ =
  { name = "A"; alias_of = ["Lib__a"]; args = []; signature = asig };;

let b: module_ =
  { name = "B"; alias_of = ["Lib__b"]; args = []; signature = empty_sig };;

let access l = Access (Name.Set.of_list l);;

let code : m2l = [ access ["A"]; Defs asig  ];;

normalize code;;
