

(** Format type *)
type format =
  | Src (** standard source file *)
  | M2l (** M2l serialized file *)
  | Parsetree (** parsetree ast file *)
  | Cmi

(** Extend M2l.kind to include the format of read file *)
type kind = { format: format; kind: M2l.kind }


type error = Ocaml of Syntaxerr.error | M2l

let name filename = String.capitalize_ascii @@
  Filename.chop_extension @@ Filename.basename filename

let ok x = Ok x

let source_file kind filename =
  Location.input_name := filename;
  let input_file = Pparse.preprocess filename in
  let code =  try ok @@
      match kind with
      | M2l.Structure ->
        Ast_converter.structure @@
        Pparse.parse_implementation Format.err_formatter ~tool_name:"codept"
          input_file
      | M2l.Signature ->
        Ast_converter.signature @@
        Pparse.parse_interface Format.err_formatter ~tool_name:"codept" input_file
    with Syntaxerr.Error msg ->
      Error (Ocaml msg)
  in
  Pparse.remove_preprocessed input_file;
  code

let file {format;kind} filename =
  let name = name filename in
  name,
  match format with
  | Src | Parsetree -> source_file kind filename
  | M2l ->
    let file = open_in filename in
    let lex = Lexing.from_channel file in
    begin
      (*    match M2l.sexp.parse @@ Sexp_parse.many Sexp_lex.main lex with*)
      match Schematic.Full.strict Schema.m2l @@ Sparser.main Slex.main lex with
      | Some m2l -> close_in file; ok m2l
      | None -> close_in file; Error M2l
      | exception Parsing.Parse_error -> close_in file; Error M2l
    end
  | Cmi  -> ok @@ Cmi.m2l filename
