let signature= let open Module in  let open Sig in 
of_list [M {name="Arith_flags"; precision=Exact; origin=Unit {source=Special "stdlib/num"; file=["Arith_flags"]}; args=[]; signature=empty}; 
        M {name="Arith_status"; precision=Exact; origin=Unit {source=Special "stdlib/num"; file=["Arith_status"]}; args=[]; signature=empty}; 
        M {name="Big_int"; precision=Exact; origin=Unit {source=Special "stdlib/num"; file=["Big_int"]}; args=[]; signature=empty}; 
        M {name="Int_misc"; precision=Exact; origin=Unit {source=Special "stdlib/num"; file=["Int_misc"]}; args=[]; signature=empty}; 
        M {name="Nat"; precision=Exact; origin=Unit {source=Special "stdlib/num"; file=["Nat"]}; args=[]; signature=empty}; 
        M {name="Num"; precision=Exact; origin=Unit {source=Special "stdlib/num"; file=["Num"]}; args=[]; signature=empty}; 
        M {name="Ratio"; precision=Exact; origin=Unit {source=Special "stdlib/num"; file=["Ratio"]}; args=[]; signature=empty}]
