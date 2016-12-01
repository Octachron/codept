let signature=
  let open Module in
  let open Sig in
  of_list 
[{name="Arg"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Arg"]}; args=[]; signature=empty}; 
{name="Arith_status"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Arith_status"]}; args=[]; signature=empty}; 
{name="Array"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Array"]}; args=[]; signature=empty}; 
{name="ArrayLabels"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["ArrayLabels"]}; args=[]; signature=empty}; 
{name="Big_int"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Big_int"]}; args=[]; signature=empty}; 
{name="Bigarray"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Bigarray"]}; args=[]; signature=of_list 
[{name="Array1"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
{name="Array2"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
{name="Array3"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
{name="Genarray"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
{name="Buffer"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Buffer"]}; args=[]; signature=empty}; 
{name="Bytes"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Bytes"]}; args=[]; signature=empty}; 
{name="BytesLabels"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["BytesLabels"]}; args=[]; signature=empty}; 
{name="Callback"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Callback"]}; args=[]; signature=empty}; 
{name="CamlinternalFormat"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["CamlinternalFormat"]}; args=[]; signature=empty}; 
{name="CamlinternalFormatBasics"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["CamlinternalFormatBasics"]}; args=[]; signature=empty}; 
{name="CamlinternalLazy"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["CamlinternalLazy"]}; args=[]; signature=empty}; 
{name="CamlinternalMod"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["CamlinternalMod"]}; args=[]; signature=empty}; 
{name="CamlinternalOO"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["CamlinternalOO"]}; args=[]; signature=empty}; 
{name="Char"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Char"]}; args=[]; signature=empty}; 
{name="Complex"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Complex"]}; args=[]; signature=empty}; 
{name="Condition"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Condition"]}; args=[]; signature=empty}; 
{name="Digest"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Digest"]}; args=[]; signature=empty}; 
{name="Dynlink"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Dynlink"]}; args=[]; signature=empty}; 
{name="Ephemeron"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Ephemeron"]}; args=[]; signature=
(merge 
(of_list [{name="GenHashTable"; precision=Exact; origin=Submodule; args=[]; signature=of_list 
          [{name="MakeSeeded"; precision=Exact; origin=Submodule; args=[Some {name="H"; precision=Exact; origin=Arg; args=[]; signature=empty}]; signature=empty}]}; 
{name="K1"; precision=Exact; origin=Submodule; args=[]; signature=of_list [{name="Make"; precision=Exact; origin=Submodule; args=[Some 
                                                          {name="H"; precision=Exact; origin=Arg; args=[]; signature=empty}]; signature=empty}; 
                                                         {name="MakeSeeded"; precision=Exact; origin=Submodule; args=[Some 
                                                         {name="H"; precision=Exact; origin=Arg; args=[]; signature=empty}]; signature=empty}]}; 
{name="K2"; precision=Exact; origin=Submodule; args=[]; signature=of_list [{name="Make"; precision=Exact; origin=Submodule; args=[Some 
                                                          {name="H1"; precision=Exact; origin=Arg; args=[]; signature=empty}; 
                                                          Some {name="H2"; precision=Exact; origin=Arg; args=[]; signature=empty}]; signature=empty}; 
                                                         {name="MakeSeeded"; precision=Exact; origin=Submodule; args=[Some 
                                                         {name="H1"; precision=Exact; origin=Arg; args=[]; signature=empty}; 
                                                         Some {name="H2"; precision=Exact; origin=Arg; args=[]; signature=empty}]; signature=empty}]}; 
{name="Kn"; precision=Exact; origin=Submodule; args=[]; signature=of_list [{name="Make"; precision=Exact; origin=Submodule; args=[Some 
                                                          {name="H"; precision=Exact; origin=Arg; args=[]; signature=empty}]; signature=empty}; 
                                                         {name="MakeSeeded"; precision=Exact; origin=Submodule; args=[Some 
                                                         {name="H"; precision=Exact; origin=Arg; args=[]; signature=empty}]; signature=empty}]}]) 
(of_list_type [{name="S"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
{name="SeededS"; precision=Exact; origin=Submodule; args=[]; signature=empty}]) )}; 
{name="Event"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Event"]}; args=[]; signature=empty}; 
{name="Filename"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Filename"]}; args=[]; signature=empty}; 
{name="Format"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Format"]}; args=[]; signature=empty}; 
{name="Gc"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Gc"]}; args=[]; signature=empty}; 
{name="Genlex"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Genlex"]}; args=[]; signature=empty}; 
{name="Graphics"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Graphics"]}; args=[]; signature=empty}; 
{name="GraphicsX11"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["GraphicsX11"]}; args=[]; signature=empty}; 
{name="Hashtbl"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Hashtbl"]}; args=[]; signature=
(merge 
(of_list [{name="Make"; precision=Exact; origin=Submodule; args=[Some {name="H"; precision=Exact; origin=Arg; args=[]; signature=empty}]; signature=empty}; 
{name="MakeSeeded"; precision=Exact; origin=Submodule; args=[Some {name="H"; precision=Exact; origin=Arg; args=[]; signature=empty}]; signature=empty}]) 
(of_list_type [{name="HashedType"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
{name="S"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
{name="SeededHashedType"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
{name="SeededS"; precision=Exact; origin=Submodule; args=[]; signature=empty}]) )}; 
{name="Int32"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Int32"]}; args=[]; signature=empty}; 
{name="Int64"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Int64"]}; args=[]; signature=empty}; 
{name="Lazy"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Lazy"]}; args=[]; signature=empty}; 
{name="Lexing"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Lexing"]}; args=[]; signature=empty}; 
{name="List"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["List"]}; args=[]; signature=empty}; 
{name="ListLabels"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["ListLabels"]}; args=[]; signature=empty}; 
{name="Map"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Map"]}; args=[]; signature=
(merge 
(of_list [{name="Make"; precision=Exact; origin=Submodule; args=[Some {name="Ord"; precision=Exact; origin=Arg; args=[]; signature=empty}]; signature=empty}]) 
(of_list_type [{name="OrderedType"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
{name="S"; precision=Exact; origin=Submodule; args=[]; signature=empty}]) )}; 
{name="Marshal"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Marshal"]}; args=[]; signature=empty}; 
{name="MoreLabels"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["MoreLabels"]}; args=[]; signature=of_list 
[{name="Hashtbl"; precision=Exact; origin=Submodule; args=[]; signature=(merge 
                                                       (of_list [{name="Make"; precision=Exact; origin=Submodule; args=[Some 
                                                                 {name="H"; precision=Exact; origin=Arg; args=[]; signature=empty}]; signature=empty}; 
                                                       {name="MakeSeeded"; precision=Exact; origin=Submodule; args=[Some 
                                                       {name="H"; precision=Exact; origin=Arg; args=[]; signature=empty}]; signature=empty}]) 
                                                       (of_list_type [
                                                       {name="HashedType"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
                                                       {name="S"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
                                                       {name="SeededHashedType"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
                                                       {name="SeededS"; precision=Exact; origin=Submodule; args=[]; signature=empty}])
                                                        )}; 
{name="Map"; precision=Exact; origin=Submodule; args=[]; signature=(merge 
                                                  (of_list [{name="Make"; precision=Exact; origin=Submodule; args=[Some 
                                                            {name="Ord"; precision=Exact; origin=Arg; args=[]; signature=empty}]; signature=empty}]) 
                                                  (of_list_type [{name="OrderedType"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
                                                  {name="S"; precision=Exact; origin=Submodule; args=[]; signature=empty}])
                                                   )}; 
{name="Set"; precision=Exact; origin=Submodule; args=[]; signature=(merge 
                                                  (of_list [{name="Make"; precision=Exact; origin=Submodule; args=[Some 
                                                            {name="Ord"; precision=Exact; origin=Arg; args=[]; signature=empty}]; signature=empty}]) 
                                                  (of_list_type [{name="OrderedType"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
                                                  {name="S"; precision=Exact; origin=Submodule; args=[]; signature=empty}])
                                                   )}]}; 
{name="Mutex"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Mutex"]}; args=[]; signature=empty}; 
{name="Nat"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Nat"]}; args=[]; signature=empty}; 
{name="Nativeint"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Nativeint"]}; args=[]; signature=empty}; 
{name="Num"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Num"]}; args=[]; signature=empty}; 
{name="Obj"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Obj"]}; args=[]; signature=of_list 
[{name="Ephemeron"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
{name="Oo"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Oo"]}; args=[]; signature=empty}; 
{name="Parsing"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Parsing"]}; args=[]; signature=empty}; 
{name="Pervasives"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Pervasives"]}; args=[]; signature=of_list 
[{name="LargeFile"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
{name="Printexc"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Printexc"]}; args=[]; signature=of_list 
[{name="Slot"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
{name="Printf"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Printf"]}; args=[]; signature=empty}; 
{name="Queue"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Queue"]}; args=[]; signature=empty}; 
{name="Random"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Random"]}; args=[]; signature=of_list 
[{name="State"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
{name="Ratio"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Ratio"]}; args=[]; signature=empty}; 
{name="Raw_spacetime_lib"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Raw_spacetime_lib"]}; args=[]; signature=of_list 
[{name="Annotation"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
{name="Frame_table"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
{name="Function_entry_point"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
{name="Function_identifier"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
{name="Gc_stats"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
{name="Heap_snapshot"; precision=Exact; origin=Submodule; args=[]; signature=of_list [
                                                                    {name="Entries"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
                                                                    {name="Event"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
                                                                    {name="Series"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
                                                                    {name="Total_allocation"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
{name="Program_counter"; precision=Exact; origin=Submodule; args=[]; signature=of_list 
[{name="Foreign"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
{name="OCaml"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
{name="Shape_table"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
{name="Trace"; precision=Exact; origin=Submodule; args=[]; signature=of_list [{name="Foreign"; precision=Exact; origin=Submodule; args=[]; signature=of_list 
                                                             [{name="Allocation_point"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
                                                             {name="Call_point"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
                                                             {name="Field"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
                                                             {name="Node"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
                                                            {name="Node"; precision=Exact; origin=Submodule; args=[]; signature=of_list 
                                                            [{name="Map"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
                                                            {name="Set"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
                                                            {name="OCaml"; precision=Exact; origin=Submodule; args=[]; signature=of_list 
                                                            [{name="Allocation_point"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
                                                            {name="Direct_call_point"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
                                                            {name="Field"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
                                                            {name="Indirect_call_point"; precision=Exact; origin=Submodule; args=[]; signature=of_list 
                                                            [{name="Callee"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
                                                            {name="Node"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}]}]}; 
{name="Scanf"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Scanf"]}; args=[]; signature=of_list 
[{name="Scanning"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
{name="Set"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Set"]}; args=[]; signature=
(merge 
(of_list [{name="Make"; precision=Exact; origin=Submodule; args=[Some {name="Ord"; precision=Exact; origin=Arg; args=[]; signature=empty}]; signature=empty}]) 
(of_list_type [{name="OrderedType"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
{name="S"; precision=Exact; origin=Submodule; args=[]; signature=empty}]) )}; 
{name="Sort"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Sort"]}; args=[]; signature=empty}; 
{name="Spacetime"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Spacetime"]}; args=[]; signature=of_list 
[{name="Series"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
{name="Snapshot"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
{name="Stack"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Stack"]}; args=[]; signature=empty}; 
{name="StdLabels"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["StdLabels"]}; args=[]; signature=of_list 
[{name="Array"; precision=Exact; origin=Alias (Unit {source=Special "stdlib"; file=["ArrayLabels"]}); args=[]; signature=empty}; 
{name="Bytes"; precision=Exact; origin=Alias (Unit {source=Special "stdlib"; file=["BytesLabels"]}); args=[]; signature=empty}; 
{name="List"; precision=Exact; origin=Alias (Unit {source=Special "stdlib"; file=["ListLabels"]}); args=[]; signature=empty}; 
{name="String"; precision=Exact; origin=Alias (Unit {source=Special "stdlib"; file=["StringLabels"]}); args=[]; signature=empty}]}; 
{name="Str"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Str"]}; args=[]; signature=empty}; 
{name="Stream"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Stream"]}; args=[]; signature=empty}; 
{name="String"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["String"]}; args=[]; signature=empty}; 
{name="StringLabels"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["StringLabels"]}; args=[]; signature=empty}; 
{name="Sys"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Sys"]}; args=[]; signature=empty}; 
{name="Thread"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Thread"]}; args=[]; signature=empty}; 
{name="ThreadUnix"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["ThreadUnix"]}; args=[]; signature=empty}; 
{name="Uchar"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Uchar"]}; args=[]; signature=empty}; 
{name="Unix"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Unix"]}; args=[]; signature=of_list 
[{name="LargeFile"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
{name="UnixLabels"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["UnixLabels"]}; args=[]; signature=of_list 
[{name="LargeFile"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
{name="Weak"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Weak"]}; args=[]; signature=
(merge 
(of_list [{name="Make"; precision=Exact; origin=Submodule; args=[Some {name="H"; precision=Exact; origin=Arg; args=[]; signature=empty}]; signature=empty}]) 
(of_list_type [{name="S"; precision=Exact; origin=Submodule; args=[]; signature=empty}]) )}]
