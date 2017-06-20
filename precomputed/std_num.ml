let modules = let open Module in  let open Sig in 
Dict.of_list [M {name="Arith_flags"; origin=Unit {source=Special "stdlib/num"; file=["Arith_flags"]}; args=[]; signature=empty}; 
        M {name="Arith_status"; origin=Unit {source=Special "stdlib/num"; file=["Arith_status"]}; args=[]; signature=empty}; 
        M {name="Big_int"; origin=Unit {source=Special "stdlib/num"; file=["Big_int"]}; args=[]; signature=empty}; 
        M {name="Int_misc"; origin=Unit {source=Special "stdlib/num"; file=["Int_misc"]}; args=[]; signature=empty}; 
        M {name="Nat"; origin=Unit {source=Special "stdlib/num"; file=["Nat"]}; args=[]; signature=empty}; 
        M {name="Num"; origin=Unit {source=Special "stdlib/num"; file=["Num"]}; args=[]; signature=empty}; 
        M {name="Ratio"; origin=Unit {source=Special "stdlib/num"; file=["Ratio"]}; args=[]; signature=empty}]
