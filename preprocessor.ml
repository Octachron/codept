exception Parse_error of string

type v = {major:int; minor:int}
type version =
  | Finite of v
  | Inf

let parse_error fmt = Format.ksprintf (fun s ->  raise (Parse_error s)) fmt

type 'a parse = {pos:int; x:'a}
let scan {pos; x} = if pos < String.length x then Some x.[pos] else None
let incr {pos; x} = {pos=succ pos; x}

let rec skip c s =
  if scan s = Some c then skip c (incr s) else s
let skip_dot = skip '.'


let parse_integer =
  let rec parse_integer rem s = match scan s with
    | Some ('0'..'9' as c) ->
      let n = Char.code c - Char.code '0' in
      parse_integer (10 *rem + n) (incr s)
    | _ -> rem, s
  in
  parse_integer 0

let parse_version x =
  match scan x with
  | Some 'o' -> Inf, skip 'o' (incr x)
  | _ ->
    let major, x = parse_integer (skip ' ' x) in
    let x = skip '.' x in
    let minor, x = parse_integer x in
    Finite {major; minor}, x

type interval = {left:version; right:version}
type test = interval

let mem interval x = match interval.left, interval.right with
  | Inf , Inf -> true
  | Inf, Finite y -> x < y
  | Finite y, Inf -> y <= x
  | Finite a, Finite b -> a <= x && x < b

let (<:) x intervs = List.for_all (fun interv -> mem interv x) intervs

type annot =
  | Start of test
  | End

let sub {pos;x} s =
  String.sub x pos (String.length s) = s

let parse x =
  let x = skip ' ' x in     
  if sub x "end" then
    End
  else
    let left, x = parse_version x in
    let x = List.fold_left (fun x c -> skip c x) x [' '; ','; ' '] in
    let right, x = parse_version x in
    Start {left; right}

(*
let rec parse s = match scan s with
  | Some ' ' -> parse (incr s)
  | Some '>' -> parse_mixt Greater (incr s)
  | Some '<' -> parse_mixt Lesser (incr s)
  | Some '=' -> with_version Equal (incr s)
  | Some c when c = ge.[0] -> parse_unicode (incr s)
  | Some '\n' -> parse_error "Unexpected end of input in %s" s.x
  | Some 'e' -> if sub s "end" then End else parse_error "Unexpected character in %s" s.x
  | Some _ -> parse_error "Unexpected character in %s" s.x
  | None -> parse_error "Unexpected end"
and parse_mixt base s =
  if scan s = Some '=' then  with_version (Or(Equal,base)) (incr s)
  else with_version base s
and with_version c s = Start {c; version=parse_version s }
and parse_unicode s =
  if not (scan s=Some ge.[1]) then parse_error "Unexpected character in %s" s.x
  else
    let s = incr s in
    let base =
      let c = scan s in
      if c = Some ge.[2] then Greater
      else if c = Some le.[2] then Lesser
      else parse_error "Unexpected character in %s" s.x in
    with_version (Or(Equal,base)) (incr s)
*)

let conditional line =
  if String.length line > 0 && line.[0] = '#'
  then Some (parse {pos=1;x=line})
  else None

let version = match parse_version {pos=0;x=Sys.ocaml_version} with
  | Finite v, _ -> v
  | Inf, _ -> assert false

exception Missing_end

let line read write active =
  let line = read () in
  let macro, active =
    match conditional line with
    | None -> false, active
    | Some Start new' -> true, [new']
    | Some End -> true, []
  in
  if not macro && version <: active then write line;
  active

let rename s = String.sub s 0 (String.length s - 1)
let preprocess file =
  Printf.eprintf "Preprocessing %s to %s\n%!" file (rename file);
  let input = open_in file in
  let output = open_out (rename file) in
  let read () = input_line input in
  let write s =
    (*    Printf.eprintf "%s\n" s;*)
    output_string output s; output_string output "\n" in
  let rec loop stack =
    match line read write stack with
    | exception End_of_file ->
      if stack <> [] then raise Missing_end;
      flush output;
      close_in input; close_out output
    | stack -> loop stack in
  loop []

let files = List.map (Format.sprintf "lib/%s.mlp")
  ["ast_converter";"cmi"; "pparse_compat"; "format_compat"; "format_tags"]
  @ List.map (Format.sprintf "full/%s.mlp")
    ["extended_args"]
let () =
  List.iter preprocess files
(*  Arg.parse [] preprocess "preprocessor <file>"*)
