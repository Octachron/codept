let modules= let open Module in  let open Sig in 
Dict.of_list [M {name="Arith_flags"; origin=Unit {source={source=Special "stdlib/num"; file=["Arith_flags"]};path=["Arith_flags"]}; args=[]; signature=empty}; 
               M {name="Arith_status"; origin=Unit {source={source=Special "stdlib/num"; file=["Arith_status"]};path=["Arith_status"]}; args=[]; signature=empty}; 
               M {name="Big_int"; origin=Unit {source={source=Special "stdlib/num"; file=["Big_int"]};path=["Big_int"]}; args=[]; signature=empty}; 
               M {name="Int_misc"; origin=Unit {source={source=Special "stdlib/num"; file=["Int_misc"]};path=["Int_misc"]}; args=[]; signature=empty}; 
               M {name="Nat"; origin=Unit {source={source=Special "stdlib/num"; file=["Nat"]};path=["Nat"]}; args=[]; signature=empty}; 
               M {name="Num"; origin=Unit {source={source=Special "stdlib/num"; file=["Num"]};path=["Num"]}; args=[]; signature=empty}; 
               M {name="Ratio"; origin=Unit {source={source=Special "stdlib/num"; file=["Ratio"]};path=["Ratio"]}; args=[]; signature=empty}] 
