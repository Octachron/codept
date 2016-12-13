
let name filename = String.capitalize_ascii @@
  Filename.chop_extension @@ Filename.basename filename

let ok x = Ok x

let file kind filename =
  let name = name filename in
  Location.input_name := filename;
  let input_file = Pparse.preprocess filename in
  let code =  try ok @@
      match kind with
      | M2l.Structure ->
        Ast_converter.structure @@
        Pparse.file Format.err_formatter ~tool_name:"codept" input_file
          Parse.implementation Pparse.Structure
      | M2l.Signature ->
        Ast_converter.signature @@
        Pparse.file Format.err_formatter ~tool_name:"codept" input_file
          Parse.interface Pparse.Signature
    with Syntaxerr.Error msg ->
      Error msg
  in
  Pparse.remove_preprocessed input_file;
  name, code
