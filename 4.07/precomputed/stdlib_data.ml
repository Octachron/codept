let modules= let open Module in  let open Sig in 
Dict.of_list [M {name="CamlinternalFormat"; origin=Unit {source={source=Special "stdlib"; file=["CamlinternalFormat"]};path=["CamlinternalFormat"]}; args=[]; signature=empty};
               M {name="CamlinternalFormatBasics"; origin=Unit {source={source=Special "stdlib"; file=["CamlinternalFormatBasics"]};path=["CamlinternalFormatBasics"]}; args=[]; signature=empty};
               M {name="CamlinternalLazy"; origin=Unit {source={source=Special "stdlib"; file=["CamlinternalLazy"]};path=["CamlinternalLazy"]}; args=[]; signature=empty};
               M {name="CamlinternalMod"; origin=Unit {source={source=Special "stdlib"; file=["CamlinternalMod"]};path=["CamlinternalMod"]}; args=[]; signature=empty};
               M {name="CamlinternalOO"; origin=Unit {source={source=Special "stdlib"; file=["CamlinternalOO"]};path=["CamlinternalOO"]}; args=[]; signature=empty};
               M {name="Std_exit"; origin=Unit {source={source=Special "stdlib"; file=["Std_exit"]};path=["Std_exit"]}; args=[]; signature=empty};
               M {name="Stdlib"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib"]};path=["Stdlib"]}; args=[]; signature=of_list 
                 [Alias {name="Arg";path=Namespaced.make "Stdlib__arg";phantom=None;weak=false};
                 Alias {name="Array";path=Namespaced.make "Stdlib__array";phantom=None;weak=false};
                 Alias {name="ArrayLabels";path=Namespaced.make "Stdlib__arrayLabels";phantom=None;weak=false};
                 Alias {name="Bigarray";path=Namespaced.make "Stdlib__bigarray";phantom=None;weak=false};
                 Alias {name="Buffer";path=Namespaced.make "Stdlib__buffer";phantom=None;weak=false};
                 Alias {name="Bytes";path=Namespaced.make "Stdlib__bytes";phantom=None;weak=false};
                 Alias {name="BytesLabels";path=Namespaced.make "Stdlib__bytesLabels";phantom=None;weak=false};
                 Alias {name="Callback";path=Namespaced.make "Stdlib__callback";phantom=None;weak=false};
                 Alias {name="Char";path=Namespaced.make "Stdlib__char";phantom=None;weak=false};
                 Alias {name="Complex";path=Namespaced.make "Stdlib__complex";phantom=None;weak=false};
                 Alias {name="Digest";path=Namespaced.make "Stdlib__digest";phantom=None;weak=false};
                 Alias {name="Ephemeron";path=Namespaced.make "Stdlib__ephemeron";phantom=None;weak=false};
                 Alias {name="Filename";path=Namespaced.make "Stdlib__filename";phantom=None;weak=false};
                 Alias {name="Float";path=Namespaced.make "Stdlib__float";phantom=None;weak=false};
                 Alias {name="Format";path=Namespaced.make "Stdlib__format";phantom=None;weak=false};
                 Alias {name="Gc";path=Namespaced.make "Stdlib__gc";phantom=None;weak=false};
                 Alias {name="Genlex";path=Namespaced.make "Stdlib__genlex";phantom=None;weak=false};
                 Alias {name="Hashtbl";path=Namespaced.make "Stdlib__hashtbl";phantom=None;weak=false};
                 Alias {name="Int32";path=Namespaced.make "Stdlib__int32";phantom=None;weak=false};
                 Alias {name="Int64";path=Namespaced.make "Stdlib__int64";phantom=None;weak=false};
                 M {name="LargeFile"; origin=Submodule; args=[]; signature=empty};
                 Alias {name="Lazy";path=Namespaced.make "Stdlib__lazy";phantom=None;weak=false};
                 Alias {name="Lexing";path=Namespaced.make "Stdlib__lexing";phantom=None;weak=false};
                 Alias {name="List";path=Namespaced.make "Stdlib__list";phantom=None;weak=false};
                 Alias {name="ListLabels";path=Namespaced.make "Stdlib__listLabels";phantom=None;weak=false};
                 Alias {name="Map";path=Namespaced.make "Stdlib__map";phantom=None;weak=false};
                 Alias {name="Marshal";path=Namespaced.make "Stdlib__marshal";phantom=None;weak=false};
                 Alias {name="MoreLabels";path=Namespaced.make "Stdlib__moreLabels";phantom=None;weak=false};
                 Alias {name="Nativeint";path=Namespaced.make "Stdlib__nativeint";phantom=None;weak=false};
                 Alias {name="Obj";path=Namespaced.make "Stdlib__obj";phantom=None;weak=false};
                 Alias {name="Oo";path=Namespaced.make "Stdlib__oo";phantom=None;weak=false};
                 Alias {name="Parsing";path=Namespaced.make "Stdlib__parsing";phantom=None;weak=false};
                 M {name="Pervasives"; origin=Submodule; args=[]; signature=of_list 
                   [M {name="LargeFile"; origin=Submodule; args=[]; signature=empty}]};
                 Alias {name="Printexc";path=Namespaced.make "Stdlib__printexc";phantom=None;weak=false};
                 Alias {name="Printf";path=Namespaced.make "Stdlib__printf";phantom=None;weak=false};
                 Alias {name="Queue";path=Namespaced.make "Stdlib__queue";phantom=None;weak=false};
                 Alias {name="Random";path=Namespaced.make "Stdlib__random";phantom=None;weak=false};
                 Alias {name="Scanf";path=Namespaced.make "Stdlib__scanf";phantom=None;weak=false};
                 Alias {name="Seq";path=Namespaced.make "Stdlib__seq";phantom=None;weak=false};
                 Alias {name="Set";path=Namespaced.make "Stdlib__set";phantom=None;weak=false};
                 Alias {name="Sort";path=Namespaced.make "Stdlib__sort";phantom=None;weak=false};
                 Alias {name="Spacetime";path=Namespaced.make "Stdlib__spacetime";phantom=None;weak=false};
                 Alias {name="Stack";path=Namespaced.make "Stdlib__stack";phantom=None;weak=false};
                 Alias {name="StdLabels";path=Namespaced.make "Stdlib__stdLabels";phantom=None;weak=false};
                 Alias {name="Stream";path=Namespaced.make "Stdlib__stream";phantom=None;weak=false};
                 Alias {name="String";path=Namespaced.make "Stdlib__string";phantom=None;weak=false};
                 Alias {name="StringLabels";path=Namespaced.make "Stdlib__stringLabels";phantom=None;weak=false};
                 Alias {name="Sys";path=Namespaced.make "Stdlib__sys";phantom=None;weak=false};
                 Alias {name="Uchar";path=Namespaced.make "Stdlib__uchar";phantom=None;weak=false};
                 Alias {name="Weak";path=Namespaced.make "Stdlib__weak";phantom=None;weak=false}]};
               M {name="Stdlib__arg"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__arg"]};path=["Stdlib__arg"]}; args=[]; signature=empty};
               M {name="Stdlib__array"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__array"]};path=["Stdlib__array"]}; args=[]; signature=of_list 
                 [M {name="Floatarray"; origin=Submodule; args=[]; signature=empty}]};
               M {name="Stdlib__arrayLabels"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__arrayLabels"]};path=["Stdlib__arrayLabels"]}; args=[]; signature=of_list 
                 [M {name="Floatarray"; origin=Submodule; args=[]; signature=empty}]};
               M {name="Stdlib__bigarray"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__bigarray"]};path=["Stdlib__bigarray"]}; args=[]; signature=of_list 
                 [M {name="Array0"; origin=Submodule; args=[]; signature=empty};
                 M {name="Array1"; origin=Submodule; args=[]; signature=empty};
                 M {name="Array2"; origin=Submodule; args=[]; signature=empty};
                 M {name="Array3"; origin=Submodule; args=[]; signature=empty};
                 M {name="Genarray"; origin=Submodule; args=[]; signature=empty}]};
               M {name="Stdlib__buffer"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__buffer"]};path=["Stdlib__buffer"]}; args=[]; signature=empty};
               M {name="Stdlib__bytes"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__bytes"]};path=["Stdlib__bytes"]}; args=[]; signature=empty};
               M {name="Stdlib__bytesLabels"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__bytesLabels"]};path=["Stdlib__bytesLabels"]}; args=[]; signature=empty};
               M {name="Stdlib__callback"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__callback"]};path=["Stdlib__callback"]}; args=[]; signature=empty};
               M {name="Stdlib__char"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__char"]};path=["Stdlib__char"]}; args=[]; signature=empty};
               M {name="Stdlib__complex"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__complex"]};path=["Stdlib__complex"]}; args=[]; signature=empty};
               M {name="Stdlib__digest"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__digest"]};path=["Stdlib__digest"]}; args=[]; signature=empty};
               M {name="Stdlib__ephemeron"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__ephemeron"]};path=["Stdlib__ephemeron"]}; args=[]; signature=
                 (merge 
                 (of_list [M {name="GenHashTable"; origin=Submodule; args=[]; signature=of_list 
                             [M {name="MakeSeeded"; origin=Submodule; args=[Some 
                                {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]};
                 M {name="K1"; origin=Submodule; args=[]; signature=of_list 
                   [M {name="Make"; origin=Submodule; args=[Some {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty};
                   M {name="MakeSeeded"; origin=Submodule; args=[Some 
                     {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]};
                 M {name="K2"; origin=Submodule; args=[]; signature=of_list 
                   [M {name="Make"; origin=Submodule; args=[Some {name="H1"; origin=Submodule; args=[]; signature=empty};
                      Some {name="H2"; origin=Submodule; args=[]; signature=empty}]; signature=empty};
                   M {name="MakeSeeded"; origin=Submodule; args=[Some 
                     {name="H1"; origin=Submodule; args=[]; signature=empty};
                     Some {name="H2"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]};
                 M {name="Kn"; origin=Submodule; args=[]; signature=of_list 
                   [M {name="Make"; origin=Submodule; args=[Some {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty};
                   M {name="MakeSeeded"; origin=Submodule; args=[Some 
                     {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]}]) 
                 (of_list_type [M {name="S"; origin=Submodule; args=[]; signature=empty};
                 M {name="SeededS"; origin=Submodule; args=[]; signature=empty}])
                  )};
               M {name="Stdlib__filename"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__filename"]};path=["Stdlib__filename"]}; args=[]; signature=empty};
               M {name="Stdlib__float"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__float"]};path=["Stdlib__float"]}; args=[]; signature=of_list 
                 [M {name="Array"; origin=Submodule; args=[]; signature=empty}]};
               M {name="Stdlib__format"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__format"]};path=["Stdlib__format"]}; args=[]; signature=empty};
               M {name="Stdlib__gc"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__gc"]};path=["Stdlib__gc"]}; args=[]; signature=empty};
               M {name="Stdlib__genlex"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__genlex"]};path=["Stdlib__genlex"]}; args=[]; signature=empty};
               M {name="Stdlib__hashtbl"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__hashtbl"]};path=["Stdlib__hashtbl"]}; args=[]; signature=
                 (merge 
                 (of_list [M {name="Make"; origin=Submodule; args=[Some 
                             {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty};
                 M {name="MakeSeeded"; origin=Submodule; args=[Some {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
                 (of_list_type [M {name="HashedType"; origin=Submodule; args=[]; signature=empty};
                 M {name="S"; origin=Submodule; args=[]; signature=empty};
                 M {name="SeededHashedType"; origin=Submodule; args=[]; signature=empty};
                 M {name="SeededS"; origin=Submodule; args=[]; signature=empty}])
                  )};
               M {name="Stdlib__int32"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__int32"]};path=["Stdlib__int32"]}; args=[]; signature=empty};
               M {name="Stdlib__int64"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__int64"]};path=["Stdlib__int64"]}; args=[]; signature=empty};
               M {name="Stdlib__lazy"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__lazy"]};path=["Stdlib__lazy"]}; args=[]; signature=empty};
               M {name="Stdlib__lexing"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__lexing"]};path=["Stdlib__lexing"]}; args=[]; signature=empty};
               M {name="Stdlib__list"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__list"]};path=["Stdlib__list"]}; args=[]; signature=empty};
               M {name="Stdlib__listLabels"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__listLabels"]};path=["Stdlib__listLabels"]}; args=[]; signature=empty};
               M {name="Stdlib__map"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__map"]};path=["Stdlib__map"]}; args=[]; signature=
                 (merge 
                 (of_list [M {name="Make"; origin=Submodule; args=[Some 
                             {name="Ord"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
                 (of_list_type [M {name="OrderedType"; origin=Submodule; args=[]; signature=empty};
                 M {name="S"; origin=Submodule; args=[]; signature=empty}]) )};
               M {name="Stdlib__marshal"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__marshal"]};path=["Stdlib__marshal"]}; args=[]; signature=empty};
               M {name="Stdlib__moreLabels"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__moreLabels"]};path=["Stdlib__moreLabels"]}; args=[]; signature=of_list 
                 [M {name="Hashtbl"; origin=Submodule; args=[]; signature=
                    (merge 
                    (of_list [M {name="Make"; origin=Submodule; args=[Some 
                                {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty};
                    M {name="MakeSeeded"; origin=Submodule; args=[Some 
                      {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
                    (of_list_type [M {name="HashedType"; origin=Submodule; args=[]; signature=empty};
                    M {name="S"; origin=Submodule; args=[]; signature=empty};
                    M {name="SeededHashedType"; origin=Submodule; args=[]; signature=empty};
                    M {name="SeededS"; origin=Submodule; args=[]; signature=empty}])
                     )};
                 M {name="Map"; origin=Submodule; args=[]; signature=
                   (merge 
                   (of_list [M {name="Make"; origin=Submodule; args=[Some 
                               {name="Ord"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
                   (of_list_type [M {name="OrderedType"; origin=Submodule; args=[]; signature=empty};
                   M {name="S"; origin=Submodule; args=[]; signature=empty}])
                    )};
                 M {name="Set"; origin=Submodule; args=[]; signature=
                   (merge 
                   (of_list [M {name="Make"; origin=Submodule; args=[Some 
                               {name="Ord"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
                   (of_list_type [M {name="OrderedType"; origin=Submodule; args=[]; signature=empty};
                   M {name="S"; origin=Submodule; args=[]; signature=empty}])
                    )}]};
               M {name="Stdlib__nativeint"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__nativeint"]};path=["Stdlib__nativeint"]}; args=[]; signature=empty};
               M {name="Stdlib__obj"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__obj"]};path=["Stdlib__obj"]}; args=[]; signature=of_list 
                 [M {name="Ephemeron"; origin=Submodule; args=[]; signature=empty}]};
               M {name="Stdlib__oo"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__oo"]};path=["Stdlib__oo"]}; args=[]; signature=empty};
               M {name="Stdlib__parsing"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__parsing"]};path=["Stdlib__parsing"]}; args=[]; signature=empty};
               M {name="Stdlib__printexc"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__printexc"]};path=["Stdlib__printexc"]}; args=[]; signature=of_list 
                 [M {name="Slot"; origin=Submodule; args=[]; signature=empty}]};
               M {name="Stdlib__printf"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__printf"]};path=["Stdlib__printf"]}; args=[]; signature=empty};
               M {name="Stdlib__queue"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__queue"]};path=["Stdlib__queue"]}; args=[]; signature=empty};
               M {name="Stdlib__random"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__random"]};path=["Stdlib__random"]}; args=[]; signature=of_list 
                 [M {name="State"; origin=Submodule; args=[]; signature=empty}]};
               M {name="Stdlib__scanf"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__scanf"]};path=["Stdlib__scanf"]}; args=[]; signature=of_list 
                 [M {name="Scanning"; origin=Submodule; args=[]; signature=empty}]};
               M {name="Stdlib__seq"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__seq"]};path=["Stdlib__seq"]}; args=[]; signature=empty};
               M {name="Stdlib__set"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__set"]};path=["Stdlib__set"]}; args=[]; signature=
                 (merge 
                 (of_list [M {name="Make"; origin=Submodule; args=[Some 
                             {name="Ord"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
                 (of_list_type [M {name="OrderedType"; origin=Submodule; args=[]; signature=empty};
                 M {name="S"; origin=Submodule; args=[]; signature=empty}]) )};
               M {name="Stdlib__sort"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__sort"]};path=["Stdlib__sort"]}; args=[]; signature=empty};
               M {name="Stdlib__spacetime"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__spacetime"]};path=["Stdlib__spacetime"]}; args=[]; signature=of_list 
                 [M {name="Series"; origin=Submodule; args=[]; signature=empty};
                 M {name="Snapshot"; origin=Submodule; args=[]; signature=empty}]};
               M {name="Stdlib__stack"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__stack"]};path=["Stdlib__stack"]}; args=[]; signature=empty};
               M {name="Stdlib__stdLabels"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__stdLabels"]};path=["Stdlib__stdLabels"]}; args=[]; signature=of_list 
                 [Alias {name="Array";path=Namespaced.make ~nms:["Stdlib"] "ArrayLabels";phantom=None;weak=false};
                 Alias {name="Bytes";path=Namespaced.make ~nms:["Stdlib"] "BytesLabels";phantom=None;weak=false};
                 Alias {name="List";path=Namespaced.make ~nms:["Stdlib"] "ListLabels";phantom=None;weak=false};
                 Alias {name="String";path=Namespaced.make ~nms:["Stdlib"] "StringLabels";phantom=None;weak=false}]};
               M {name="Stdlib__stream"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__stream"]};path=["Stdlib__stream"]}; args=[]; signature=empty};
               M {name="Stdlib__string"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__string"]};path=["Stdlib__string"]}; args=[]; signature=empty};
               M {name="Stdlib__stringLabels"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__stringLabels"]};path=["Stdlib__stringLabels"]}; args=[]; signature=empty};
               M {name="Stdlib__sys"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__sys"]};path=["Stdlib__sys"]}; args=[]; signature=empty};
               M {name="Stdlib__uchar"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__uchar"]};path=["Stdlib__uchar"]}; args=[]; signature=empty};
               M {name="Stdlib__weak"; origin=Unit {source={source=Special "stdlib"; file=["Stdlib__weak"]};path=["Stdlib__weak"]}; args=[]; signature=
                 (merge 
                 (of_list [M {name="Make"; origin=Submodule; args=[Some 
                             {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
                 (of_list_type [M {name="S"; origin=Submodule; args=[]; signature=empty}])
                  )}] 
