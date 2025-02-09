let modules= let open Module in  let open Sig in 
Dict.of_list [("Arg",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Arg");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Arg");namespace=["Stdlib"];}}; signature=empty}));
               ("Array",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Array");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Array");namespace=["Stdlib"];}}; signature=of_list 
                             [("Floatarray",Sig ({origin=Submodule; signature=empty}))]}));
               ("ArrayLabels",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "ArrayLabels");namespace=["Stdlib"];}};path={name=(Unitname.modulize "ArrayLabels");namespace=["Stdlib"];}}; signature=of_list 
                                   [("Floatarray",Sig ({origin=Submodule; signature=empty}))]}));
               ("Atomic",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Atomic");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Atomic");namespace=["Stdlib"];}}; signature=empty}));
               ("Bigarray",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Bigarray");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Bigarray");namespace=["Stdlib"];}}; signature=of_list 
                                [("Array0",Sig ({origin=Submodule; signature=empty}));
                                ("Array1",Sig ({origin=Submodule; signature=empty}));
                                ("Array2",Sig ({origin=Submodule; signature=empty}));
                                ("Array3",Sig ({origin=Submodule; signature=empty}));
                                ("Genarray",Sig ({origin=Submodule; signature=empty}))]}));
               ("Bool",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Bool");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Bool");namespace=["Stdlib"];}}; signature=empty}));
               ("Buffer",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Buffer");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Buffer");namespace=["Stdlib"];}}; signature=empty}));
               ("Bytes",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Bytes");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Bytes");namespace=["Stdlib"];}}; signature=empty}));
               ("BytesLabels",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "BytesLabels");namespace=["Stdlib"];}};path={name=(Unitname.modulize "BytesLabels");namespace=["Stdlib"];}}; signature=empty}));
               ("Callback",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Callback");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Callback");namespace=["Stdlib"];}}; signature=empty}));
               ("CamlinternalFormat",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "CamlinternalFormat");namespace=["Stdlib"];}};path={name=(Unitname.modulize "CamlinternalFormat");namespace=["Stdlib"];}}; signature=empty}));
               ("CamlinternalFormatBasics",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "CamlinternalFormatBasics");namespace=["Stdlib"];}};path={name=(Unitname.modulize "CamlinternalFormatBasics");namespace=["Stdlib"];}}; signature=empty}));
               ("CamlinternalLazy",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "CamlinternalLazy");namespace=["Stdlib"];}};path={name=(Unitname.modulize "CamlinternalLazy");namespace=["Stdlib"];}}; signature=empty}));
               ("CamlinternalMod",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "CamlinternalMod");namespace=["Stdlib"];}};path={name=(Unitname.modulize "CamlinternalMod");namespace=["Stdlib"];}}; signature=empty}));
               ("CamlinternalOO",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "CamlinternalOO");namespace=["Stdlib"];}};path={name=(Unitname.modulize "CamlinternalOO");namespace=["Stdlib"];}}; signature=empty}));
               ("Char",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Char");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Char");namespace=["Stdlib"];}}; signature=empty}));
               ("Complex",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Complex");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Complex");namespace=["Stdlib"];}}; signature=empty}));
               ("Condition",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Condition");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Condition");namespace=["Stdlib"];}}; signature=empty}));
               ("Digest",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Digest");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Digest");namespace=["Stdlib"];}}; signature=
                              (merge 
                              (of_list [("BLAKE128",Sig ({origin=Submodule; signature=empty}));
                              ("BLAKE256",Sig ({origin=Submodule; signature=empty}));
                              ("BLAKE512",Sig ({origin=Submodule; signature=empty}));
                              ("MD5",Sig ({origin=Submodule; signature=empty}))]) 
                              (of_list_type [("S",Sig ({origin=Submodule; signature=empty}))])
                               )}));
               ("Domain",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Domain");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Domain");namespace=["Stdlib"];}}; signature=of_list 
                              [("DLS",Sig ({origin=Submodule; signature=empty}))]}));
               ("Dynarray",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Dynarray");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Dynarray");namespace=["Stdlib"];}}; signature=empty}));
               ("Effect",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Effect");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Effect");namespace=["Stdlib"];}}; signature=of_list 
                              [("Deep",Sig ({origin=Submodule; signature=empty}));
                              ("Shallow",Sig ({origin=Submodule; signature=empty}))]}));
               ("Either",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Either");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Either");namespace=["Stdlib"];}}; signature=empty}));
               ("Ephemeron",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Ephemeron");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Ephemeron");namespace=["Stdlib"];}}; signature=
                                 (merge 
                                 (of_list [("K1",Sig ({origin=Submodule; signature=of_list 
                                                      [("Bucket",Sig (
                                                      {origin=Submodule; signature=empty}));
                                                      ("Make",Fun (Some {name=Some "H";signature=Sig (
                                                      {origin=Submodule; signature=empty})},Sig (
                                                      {origin=Submodule; signature=empty})));
                                                      ("MakeSeeded",Fun (Some {name=Some "H";signature=Sig (
                                                      {origin=Submodule; signature=empty})},Sig (
                                                      {origin=Submodule; signature=empty})))]}));
                                 ("K2",Sig ({origin=Submodule; signature=of_list 
                                            [("Bucket",Sig ({origin=Submodule; signature=empty}));
                                            ("Make",Fun (Some {name=Some "H1";signature=Sig (
                                            {origin=Submodule; signature=empty})},Fun (Some {name=Some "H2";signature=Sig (
                                            {origin=Submodule; signature=empty})},Sig (
                                            {origin=Submodule; signature=empty}))));
                                            ("MakeSeeded",Fun (Some {name=Some "H1";signature=Sig (
                                            {origin=Submodule; signature=empty})},Fun (Some {name=Some "H2";signature=Sig (
                                            {origin=Submodule; signature=empty})},Sig (
                                            {origin=Submodule; signature=empty}))))]}));
                                 ("Kn",Sig ({origin=Submodule; signature=of_list 
                                            [("Bucket",Sig ({origin=Submodule; signature=empty}));
                                            ("Make",Fun (Some {name=Some "H";signature=Sig (
                                            {origin=Submodule; signature=empty})},Sig (
                                            {origin=Submodule; signature=empty})));
                                            ("MakeSeeded",Fun (Some {name=Some "H";signature=Sig (
                                            {origin=Submodule; signature=empty})},Sig (
                                            {origin=Submodule; signature=empty})))]}))]) 
                                 (of_list_type [("S",Sig ({origin=Submodule; signature=empty}));
                                 ("SeededS",Sig ({origin=Submodule; signature=empty}))])
                                  )}));
               ("Filename",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Filename");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Filename");namespace=["Stdlib"];}}; signature=empty}));
               ("Float",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Float");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Float");namespace=["Stdlib"];}}; signature=of_list 
                             [("Array",Sig ({origin=Submodule; signature=empty}));
                             ("ArrayLabels",Sig ({origin=Submodule; signature=empty}))]}));
               ("Format",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Format");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Format");namespace=["Stdlib"];}}; signature=empty}));
               ("Fun",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Fun");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Fun");namespace=["Stdlib"];}}; signature=empty}));
               ("Gc",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Gc");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Gc");namespace=["Stdlib"];}}; signature=of_list 
                          [("Memprof",Sig ({origin=Submodule; signature=empty}))]}));
               ("Hashtbl",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Hashtbl");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Hashtbl");namespace=["Stdlib"];}}; signature=
                               (merge 
                               (of_list [("Make",Fun (Some {name=Some "H";signature=Sig (
                               {origin=Submodule; signature=empty})},Sig (
                               {origin=Submodule; signature=empty})));
                               ("MakeSeeded",Fun (Some {name=Some "H";signature=Sig (
                               {origin=Submodule; signature=empty})},Sig (
                               {origin=Submodule; signature=empty})))]) 
                               (of_list_type [("HashedType",Sig ({origin=Submodule; signature=empty}));
                               ("S",Sig ({origin=Submodule; signature=empty}));
                               ("SeededHashedType",Sig ({origin=Submodule; signature=empty}));
                               ("SeededS",Sig ({origin=Submodule; signature=empty}))])
                                )}));
               ("In_channel",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "In_channel");namespace=["Stdlib"];}};path={name=(Unitname.modulize "In_channel");namespace=["Stdlib"];}}; signature=empty}));
               ("Int",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Int");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Int");namespace=["Stdlib"];}}; signature=empty}));
               ("Int32",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Int32");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Int32");namespace=["Stdlib"];}}; signature=empty}));
               ("Int64",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Int64");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Int64");namespace=["Stdlib"];}}; signature=empty}));
               ("Lazy",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Lazy");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Lazy");namespace=["Stdlib"];}}; signature=empty}));
               ("Lexing",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Lexing");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Lexing");namespace=["Stdlib"];}}; signature=empty}));
               ("List",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "List");namespace=["Stdlib"];}};path={name=(Unitname.modulize "List");namespace=["Stdlib"];}}; signature=empty}));
               ("ListLabels",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "ListLabels");namespace=["Stdlib"];}};path={name=(Unitname.modulize "ListLabels");namespace=["Stdlib"];}}; signature=empty}));
               ("Map",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Map");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Map");namespace=["Stdlib"];}}; signature=
                           (merge 
                           (of_list [("Make",Fun (Some {name=Some "Ord";signature=Sig (
                           {origin=Submodule; signature=empty})},Sig (
                           {origin=Submodule; signature=empty})))]) 
                           (of_list_type [("OrderedType",Sig ({origin=Submodule; signature=empty}));
                           ("S",Sig ({origin=Submodule; signature=empty}))])
                            )}));
               ("Marshal",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Marshal");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Marshal");namespace=["Stdlib"];}}; signature=empty}));
               ("MoreLabels",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "MoreLabels");namespace=["Stdlib"];}};path={name=(Unitname.modulize "MoreLabels");namespace=["Stdlib"];}}; signature=of_list 
                                  [("Hashtbl",Sig ({origin=Submodule; signature=
                                                   (merge 
                                                   (of_list [("Make",Fun (Some {name=Some "H";signature=Sig (
                                                   {origin=Submodule; signature=empty})},Sig (
                                                   {origin=Submodule; signature=empty})));
                                                   ("MakeSeeded",Fun (Some {name=Some "H";signature=Sig (
                                                   {origin=Submodule; signature=empty})},Sig (
                                                   {origin=Submodule; signature=empty})))]) 
                                                   (of_list_type [("HashedType",Sig (
                                                   {origin=Submodule; signature=empty}));
                                                   ("S",Sig ({origin=Submodule; signature=empty}));
                                                   ("SeededHashedType",Sig (
                                                   {origin=Submodule; signature=empty}));
                                                   ("SeededS",Sig ({origin=Submodule; signature=empty}))])
                                                    )}));
                                  ("Map",Sig ({origin=Submodule; signature=
                                              (merge 
                                              (of_list [("Make",Fun (Some {name=Some "Ord";signature=Sig (
                                              {origin=Submodule; signature=empty})},Sig (
                                              {origin=Submodule; signature=empty})))]) 
                                              (of_list_type [("OrderedType",Sig (
                                              {origin=Submodule; signature=empty}));
                                              ("S",Sig ({origin=Submodule; signature=empty}))])
                                               )}));
                                  ("Set",Sig ({origin=Submodule; signature=
                                              (merge 
                                              (of_list [("Make",Fun (Some {name=Some "Ord";signature=Sig (
                                              {origin=Submodule; signature=empty})},Sig (
                                              {origin=Submodule; signature=empty})))]) 
                                              (of_list_type [("OrderedType",Sig (
                                              {origin=Submodule; signature=empty}));
                                              ("S",Sig ({origin=Submodule; signature=empty}))])
                                               )}))]}));
               ("Mutex",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Mutex");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Mutex");namespace=["Stdlib"];}}; signature=empty}));
               ("Nativeint",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Nativeint");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Nativeint");namespace=["Stdlib"];}}; signature=empty}));
               ("Obj",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Obj");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Obj");namespace=["Stdlib"];}}; signature=of_list 
                           [("Ephemeron",Sig ({origin=Submodule; signature=empty}));
                           ("Extension_constructor",Sig ({origin=Submodule; signature=empty}))]}));
               ("Oo",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Oo");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Oo");namespace=["Stdlib"];}}; signature=empty}));
               ("Option",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Option");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Option");namespace=["Stdlib"];}}; signature=empty}));
               ("Out_channel",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Out_channel");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Out_channel");namespace=["Stdlib"];}}; signature=empty}));
               ("Parsing",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Parsing");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Parsing");namespace=["Stdlib"];}}; signature=empty}));
               ("Printexc",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Printexc");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Printexc");namespace=["Stdlib"];}}; signature=of_list 
                                [("Slot",Sig ({origin=Submodule; signature=empty}))]}));
               ("Printf",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Printf");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Printf");namespace=["Stdlib"];}}; signature=empty}));
               ("Queue",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Queue");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Queue");namespace=["Stdlib"];}}; signature=empty}));
               ("Random",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Random");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Random");namespace=["Stdlib"];}}; signature=of_list 
                              [("State",Sig ({origin=Submodule; signature=empty}))]}));
               ("Result",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Result");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Result");namespace=["Stdlib"];}}; signature=empty}));
               ("Scanf",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Scanf");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Scanf");namespace=["Stdlib"];}}; signature=of_list 
                             [("Scanning",Sig ({origin=Submodule; signature=empty}))]}));
               ("Semaphore",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Semaphore");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Semaphore");namespace=["Stdlib"];}}; signature=of_list 
                                 [("Binary",Sig ({origin=Submodule; signature=empty}));
                                 ("Counting",Sig ({origin=Submodule; signature=empty}))]}));
               ("Seq",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Seq");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Seq");namespace=["Stdlib"];}}; signature=empty}));
               ("Set",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Set");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Set");namespace=["Stdlib"];}}; signature=
                           (merge 
                           (of_list [("Make",Fun (Some {name=Some "Ord";signature=Sig (
                           {origin=Submodule; signature=empty})},Sig (
                           {origin=Submodule; signature=empty})))]) 
                           (of_list_type [("OrderedType",Sig ({origin=Submodule; signature=empty}));
                           ("S",Sig ({origin=Submodule; signature=empty}))])
                            )}));
               ("Stack",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Stack");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Stack");namespace=["Stdlib"];}}; signature=empty}));
               ("StdLabels",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "StdLabels");namespace=["Stdlib"];}};path={name=(Unitname.modulize "StdLabels");namespace=["Stdlib"];}}; signature=of_list 
                                 [("Array",Alias {path=Namespaced.make "ArrayLabels";phantom=None});
                                 ("Bytes",Alias {path=Namespaced.make "BytesLabels";phantom=None});
                                 ("List",Alias {path=Namespaced.make "ListLabels";phantom=None});
                                 ("String",Alias {path=Namespaced.make "StringLabels";phantom=None})]}));
               ("String",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "String");namespace=["Stdlib"];}};path={name=(Unitname.modulize "String");namespace=["Stdlib"];}}; signature=empty}));
               ("StringLabels",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "StringLabels");namespace=["Stdlib"];}};path={name=(Unitname.modulize "StringLabels");namespace=["Stdlib"];}}; signature=empty}));
               ("Sys",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Sys");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Sys");namespace=["Stdlib"];}}; signature=of_list 
                           [("Immediate64",Sig ({origin=Submodule; signature=
                                                (merge 
                                                (of_list [("Make",Fun (Some {name=Some "Immediate";signature=Sig (
                                                {origin=Submodule; signature=empty})},Fun (Some {name=Some "Non_immediate";signature=Sig (
                                                {origin=Submodule; signature=empty})},Sig (
                                                {origin=Submodule; signature=empty}))))]) 
                                                (of_list_type [("Immediate",Sig (
                                                {origin=Submodule; signature=empty}));
                                                ("Non_immediate",Sig (
                                                {origin=Submodule; signature=empty}))])
                                                 )}))]}));
               ("Type",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Type");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Type");namespace=["Stdlib"];}}; signature=of_list 
                            [("Id",Sig ({origin=Submodule; signature=empty}))]}));
               ("Uchar",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Uchar");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Uchar");namespace=["Stdlib"];}}; signature=empty}));
               ("Unit",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Unit");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Unit");namespace=["Stdlib"];}}; signature=empty}));
               ("Weak",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=(Unitname.modulize "Weak");namespace=["Stdlib"];}};path={name=(Unitname.modulize "Weak");namespace=["Stdlib"];}}; signature=
                            (merge 
                            (of_list [("Make",Fun (Some {name=Some "H";signature=Sig (
                            {origin=Submodule; signature=empty})},Sig (
                            {origin=Submodule; signature=empty})))]) 
                            (of_list_type [("S",Sig ({origin=Submodule; signature=empty}))])
                             )}))] 
