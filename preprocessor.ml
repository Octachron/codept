type v = {major:int; minor:int} [@@warning "-69"]
type version =
  | Finite of v
  | Inf

type 'a parse = {pos:int; x:'a}
let scan {pos; x} = if pos < String.length x then Some x.[pos] else None
let incr {pos; x} = {pos=succ pos; x}

let rec skip c s =
  if scan s = Some c then skip c (incr s) else s

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
    let right, _ = parse_version x in
    Start {left; right}

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
   macro, active

let before s = s ^ "p"
let preprocess file =
  let src = before file in
  let input = open_in_bin src in
  let output = open_out_bin file in
  let read () = input_line input in
  let write s =
    (*    Printf.eprintf "%s\n" s;*)
    output_string output s; output_string output "\n" in
  let sync n = write (Printf.sprintf {|#%d %S|} n src )in
  sync 0;
  let rec loop active_macro ln stack =
    match line read write stack with
    | exception End_of_file ->
      if stack <> [] then raise Missing_end;
      flush output;
      close_in input; close_out output
    | macro, stack ->
      if not macro && active_macro then
        sync ln;
      loop macro (ln+1) stack
  in
  loop false 0 []

let files = List.map (Format.sprintf "lib/%s.ml")
  ["ast_converter";"cmi"; "pparse_compat"; "format_compat"; "format_tags"]
  @ List.map (Format.sprintf "full/%s.ml")
    ["extended_args"]

let targets = ref []
let () =
  Arg.parse [] (fun x->targets:= x :: !targets) "preprocessor <file>";
  let targets=
    match !targets with
    | [] -> files
    | x -> x in
   List.iter preprocess targets
