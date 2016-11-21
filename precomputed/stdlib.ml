let signature=
  let open Module in
  let open Sig in
  of_list 
[{name="Arg"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Arg"]}; args=[]; signature=empty}; 
{name="Arith_status"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Arith_status"]}; args=[]; signature=empty}; 
{name="Array"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Array"]}; args=[]; signature=empty}; 
{name="ArrayLabels"; origin=Unit {source=Pkg ["*stdlib*"]; file=["ArrayLabels"]}; args=[]; signature=empty}; 
{name="Big_int"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Big_int"]}; args=[]; signature=empty}; 
{name="Bigarray"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Bigarray"]}; args=[]; signature=of_list 
[{name="Array1"; origin=Submodule; args=[]; signature=empty}; 
{name="Array2"; origin=Submodule; args=[]; signature=empty}; 
{name="Array3"; origin=Submodule; args=[]; signature=empty}; 
{name="Genarray"; origin=Submodule; args=[]; signature=empty}]}; 
{name="Buffer"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Buffer"]}; args=[]; signature=empty}; 
{name="Bytes"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Bytes"]}; args=[]; signature=empty}; 
{name="BytesLabels"; origin=Unit {source=Pkg ["*stdlib*"]; file=["BytesLabels"]}; args=[]; signature=empty}; 
{name="Callback"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Callback"]}; args=[]; signature=empty}; 
{name="CamlinternalFormat"; origin=Unit {source=Pkg ["*stdlib*"]; file=["CamlinternalFormat"]}; args=[]; signature=empty}; 
{name="CamlinternalFormatBasics"; origin=Unit {source=Pkg ["*stdlib*"]; file=["CamlinternalFormatBasics"]}; args=[]; signature=empty}; 
{name="CamlinternalLazy"; origin=Unit {source=Pkg ["*stdlib*"]; file=["CamlinternalLazy"]}; args=[]; signature=empty}; 
{name="CamlinternalMod"; origin=Unit {source=Pkg ["*stdlib*"]; file=["CamlinternalMod"]}; args=[]; signature=empty}; 
{name="CamlinternalOO"; origin=Unit {source=Pkg ["*stdlib*"]; file=["CamlinternalOO"]}; args=[]; signature=empty}; 
{name="Char"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Char"]}; args=[]; signature=empty}; 
{name="Complex"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Complex"]}; args=[]; signature=empty}; 
{name="Condition"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Condition"]}; args=[]; signature=empty}; 
{name="Digest"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Digest"]}; args=[]; signature=empty}; 
{name="Dynlink"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Dynlink"]}; args=[]; signature=empty}; 
{name="Ephemeron"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Ephemeron"]}; args=[]; signature=
(merge 
(of_list [{name="GenHashTable"; origin=Submodule; args=[]; signature=of_list 
          [{name="MakeSeeded"; origin=Submodule; args=[Some {name="H"; origin=Arg; args=[]; signature=empty}]; signature=empty}]}; 
{name="K1"; origin=Submodule; args=[]; signature=of_list [{name="Make"; origin=Submodule; args=[Some 
                                                          {name="H"; origin=Arg; args=[]; signature=empty}]; signature=empty}; 
                                                         {name="MakeSeeded"; origin=Submodule; args=[Some 
                                                         {name="H"; origin=Arg; args=[]; signature=empty}]; signature=empty}]}; 
{name="K2"; origin=Submodule; args=[]; signature=of_list [{name="Make"; origin=Submodule; args=[Some 
                                                          {name="H1"; origin=Arg; args=[]; signature=empty}; 
                                                          Some {name="H2"; origin=Arg; args=[]; signature=empty}]; signature=empty}; 
                                                         {name="MakeSeeded"; origin=Submodule; args=[Some 
                                                         {name="H1"; origin=Arg; args=[]; signature=empty}; 
                                                         Some {name="H2"; origin=Arg; args=[]; signature=empty}]; signature=empty}]}; 
{name="Kn"; origin=Submodule; args=[]; signature=of_list [{name="Make"; origin=Submodule; args=[Some 
                                                          {name="H"; origin=Arg; args=[]; signature=empty}]; signature=empty}; 
                                                         {name="MakeSeeded"; origin=Submodule; args=[Some 
                                                         {name="H"; origin=Arg; args=[]; signature=empty}]; signature=empty}]}]) 
(of_list_type [{name="S"; origin=Submodule; args=[]; signature=empty}; 
{name="SeededS"; origin=Submodule; args=[]; signature=empty}]) )}; 
{name="Event"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Event"]}; args=[]; signature=empty}; 
{name="Filename"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Filename"]}; args=[]; signature=empty}; 
{name="Format"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Format"]}; args=[]; signature=empty}; 
{name="Gc"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Gc"]}; args=[]; signature=empty}; 
{name="Genlex"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Genlex"]}; args=[]; signature=empty}; 
{name="Graphics"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Graphics"]}; args=[]; signature=empty}; 
{name="GraphicsX11"; origin=Unit {source=Pkg ["*stdlib*"]; file=["GraphicsX11"]}; args=[]; signature=empty}; 
{name="Hashtbl"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Hashtbl"]}; args=[]; signature=
(merge 
(of_list [{name="Make"; origin=Submodule; args=[Some {name="H"; origin=Arg; args=[]; signature=empty}]; signature=empty}; 
{name="MakeSeeded"; origin=Submodule; args=[Some {name="H"; origin=Arg; args=[]; signature=empty}]; signature=empty}]) 
(of_list_type [{name="HashedType"; origin=Submodule; args=[]; signature=empty}; 
{name="S"; origin=Submodule; args=[]; signature=empty}; 
{name="SeededHashedType"; origin=Submodule; args=[]; signature=empty}; 
{name="SeededS"; origin=Submodule; args=[]; signature=empty}]) )}; 
{name="Int32"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Int32"]}; args=[]; signature=empty}; 
{name="Int64"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Int64"]}; args=[]; signature=empty}; 
{name="Lazy"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Lazy"]}; args=[]; signature=empty}; 
{name="Lexing"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Lexing"]}; args=[]; signature=empty}; 
{name="List"; origin=Unit {source=Pkg ["*stdlib*"]; file=["List"]}; args=[]; signature=empty}; 
{name="ListLabels"; origin=Unit {source=Pkg ["*stdlib*"]; file=["ListLabels"]}; args=[]; signature=empty}; 
{name="Map"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Map"]}; args=[]; signature=
(merge 
(of_list [{name="Make"; origin=Submodule; args=[Some {name="Ord"; origin=Arg; args=[]; signature=empty}]; signature=empty}]) 
(of_list_type [{name="OrderedType"; origin=Submodule; args=[]; signature=empty}; 
{name="S"; origin=Submodule; args=[]; signature=empty}]) )}; 
{name="Marshal"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Marshal"]}; args=[]; signature=empty}; 
{name="MoreLabels"; origin=Unit {source=Pkg ["*stdlib*"]; file=["MoreLabels"]}; args=[]; signature=of_list 
[{name="Hashtbl"; origin=Submodule; args=[]; signature=(merge 
                                                       (of_list [{name="Make"; origin=Submodule; args=[Some 
                                                                 {name="H"; origin=Arg; args=[]; signature=empty}]; signature=empty}; 
                                                       {name="MakeSeeded"; origin=Submodule; args=[Some 
                                                       {name="H"; origin=Arg; args=[]; signature=empty}]; signature=empty}]) 
                                                       (of_list_type [
                                                       {name="HashedType"; origin=Submodule; args=[]; signature=empty}; 
                                                       {name="S"; origin=Submodule; args=[]; signature=empty}; 
                                                       {name="SeededHashedType"; origin=Submodule; args=[]; signature=empty}; 
                                                       {name="SeededS"; origin=Submodule; args=[]; signature=empty}])
                                                        )}; 
{name="Map"; origin=Submodule; args=[]; signature=(merge 
                                                  (of_list [{name="Make"; origin=Submodule; args=[Some 
                                                            {name="Ord"; origin=Arg; args=[]; signature=empty}]; signature=empty}]) 
                                                  (of_list_type [{name="OrderedType"; origin=Submodule; args=[]; signature=empty}; 
                                                  {name="S"; origin=Submodule; args=[]; signature=empty}])
                                                   )}; 
{name="Set"; origin=Submodule; args=[]; signature=(merge 
                                                  (of_list [{name="Make"; origin=Submodule; args=[Some 
                                                            {name="Ord"; origin=Arg; args=[]; signature=empty}]; signature=empty}]) 
                                                  (of_list_type [{name="OrderedType"; origin=Submodule; args=[]; signature=empty}; 
                                                  {name="S"; origin=Submodule; args=[]; signature=empty}])
                                                   )}]}; 
{name="Mutex"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Mutex"]}; args=[]; signature=empty}; 
{name="Nat"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Nat"]}; args=[]; signature=empty}; 
{name="Nativeint"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Nativeint"]}; args=[]; signature=empty}; 
{name="Num"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Num"]}; args=[]; signature=empty}; 
{name="Obj"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Obj"]}; args=[]; signature=of_list 
[{name="Ephemeron"; origin=Submodule; args=[]; signature=empty}]}; 
{name="Oo"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Oo"]}; args=[]; signature=empty}; 
{name="Parsing"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Parsing"]}; args=[]; signature=empty}; 
{name="Pervasives"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Pervasives"]}; args=[]; signature=of_list 
[{name="LargeFile"; origin=Submodule; args=[]; signature=empty}]}; 
{name="Printexc"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Printexc"]}; args=[]; signature=of_list 
[{name="Slot"; origin=Submodule; args=[]; signature=empty}]}; 
{name="Printf"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Printf"]}; args=[]; signature=empty}; 
{name="Queue"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Queue"]}; args=[]; signature=empty}; 
{name="Random"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Random"]}; args=[]; signature=of_list 
[{name="State"; origin=Submodule; args=[]; signature=empty}]}; 
{name="Ratio"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Ratio"]}; args=[]; signature=empty}; 
{name="Raw_spacetime_lib"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Raw_spacetime_lib"]}; args=[]; signature=of_list 
[{name="Annotation"; origin=Submodule; args=[]; signature=empty}; 
{name="Frame_table"; origin=Submodule; args=[]; signature=empty}; 
{name="Function_entry_point"; origin=Submodule; args=[]; signature=empty}; 
{name="Function_identifier"; origin=Submodule; args=[]; signature=empty}; 
{name="Gc_stats"; origin=Submodule; args=[]; signature=empty}; 
{name="Heap_snapshot"; origin=Submodule; args=[]; signature=of_list [
                                                                    {name="Entries"; origin=Submodule; args=[]; signature=empty}; 
                                                                    {name="Event"; origin=Submodule; args=[]; signature=empty}; 
                                                                    {name="Series"; origin=Submodule; args=[]; signature=empty}; 
                                                                    {name="Total_allocation"; origin=Submodule; args=[]; signature=empty}]}; 
{name="Program_counter"; origin=Submodule; args=[]; signature=of_list 
[{name="Foreign"; origin=Submodule; args=[]; signature=empty}; 
{name="OCaml"; origin=Submodule; args=[]; signature=empty}]}; 
{name="Shape_table"; origin=Submodule; args=[]; signature=empty}; 
{name="Trace"; origin=Submodule; args=[]; signature=of_list [{name="Foreign"; origin=Submodule; args=[]; signature=of_list 
                                                             [{name="Allocation_point"; origin=Submodule; args=[]; signature=empty}; 
                                                             {name="Call_point"; origin=Submodule; args=[]; signature=empty}; 
                                                             {name="Field"; origin=Submodule; args=[]; signature=empty}; 
                                                             {name="Node"; origin=Submodule; args=[]; signature=empty}]}; 
                                                            {name="Node"; origin=Submodule; args=[]; signature=of_list 
                                                            [{name="Map"; origin=Submodule; args=[]; signature=empty}; 
                                                            {name="Set"; origin=Submodule; args=[]; signature=empty}]}; 
                                                            {name="OCaml"; origin=Submodule; args=[]; signature=of_list 
                                                            [{name="Allocation_point"; origin=Submodule; args=[]; signature=empty}; 
                                                            {name="Direct_call_point"; origin=Submodule; args=[]; signature=empty}; 
                                                            {name="Field"; origin=Submodule; args=[]; signature=empty}; 
                                                            {name="Indirect_call_point"; origin=Submodule; args=[]; signature=of_list 
                                                            [{name="Callee"; origin=Submodule; args=[]; signature=empty}]}; 
                                                            {name="Node"; origin=Submodule; args=[]; signature=empty}]}]}]}; 
{name="Scanf"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Scanf"]}; args=[]; signature=of_list 
[{name="Scanning"; origin=Submodule; args=[]; signature=empty}]}; 
{name="Set"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Set"]}; args=[]; signature=
(merge 
(of_list [{name="Make"; origin=Submodule; args=[Some {name="Ord"; origin=Arg; args=[]; signature=empty}]; signature=empty}]) 
(of_list_type [{name="OrderedType"; origin=Submodule; args=[]; signature=empty}; 
{name="S"; origin=Submodule; args=[]; signature=empty}]) )}; 
{name="Sort"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Sort"]}; args=[]; signature=empty}; 
{name="Spacetime"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Spacetime"]}; args=[]; signature=of_list 
[{name="Series"; origin=Submodule; args=[]; signature=empty}; 
{name="Snapshot"; origin=Submodule; args=[]; signature=empty}]}; 
{name="Stack"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Stack"]}; args=[]; signature=empty}; 
{name="StdLabels"; origin=Unit {source=Pkg ["*stdlib*"]; file=["StdLabels"]}; args=[]; signature=of_list 
[{name="Array"; origin=Alias "ArrayLabels"; args=[]; signature=empty}; 
{name="Bytes"; origin=Alias "BytesLabels"; args=[]; signature=empty}; 
{name="List"; origin=Alias "ListLabels"; args=[]; signature=empty}; 
{name="String"; origin=Alias "StringLabels"; args=[]; signature=empty}]}; 
{name="Str"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Str"]}; args=[]; signature=empty}; 
{name="Stream"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Stream"]}; args=[]; signature=empty}; 
{name="String"; origin=Unit {source=Pkg ["*stdlib*"]; file=["String"]}; args=[]; signature=empty}; 
{name="StringLabels"; origin=Unit {source=Pkg ["*stdlib*"]; file=["StringLabels"]}; args=[]; signature=empty}; 
{name="Sys"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Sys"]}; args=[]; signature=empty}; 
{name="Thread"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Thread"]}; args=[]; signature=empty}; 
{name="ThreadUnix"; origin=Unit {source=Pkg ["*stdlib*"]; file=["ThreadUnix"]}; args=[]; signature=empty}; 
{name="Uchar"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Uchar"]}; args=[]; signature=empty}; 
{name="Unix"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Unix"]}; args=[]; signature=of_list 
[{name="LargeFile"; origin=Submodule; args=[]; signature=empty}]}; 
{name="UnixLabels"; origin=Unit {source=Pkg ["*stdlib*"]; file=["UnixLabels"]}; args=[]; signature=of_list 
[{name="LargeFile"; origin=Submodule; args=[]; signature=empty}]}; 
{name="Weak"; origin=Unit {source=Pkg ["*stdlib*"]; file=["Weak"]}; args=[]; signature=
(merge 
(of_list [{name="Make"; origin=Submodule; args=[Some {name="H"; origin=Arg; args=[]; signature=empty}]; signature=empty}]) 
(of_list_type [{name="S"; origin=Submodule; args=[]; signature=empty}]) )}]
