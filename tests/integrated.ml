
(** Black-box tests
    The codept executable is called and compared to some reference results:
    Invidual cases tests/cases/%.ml are compared to tests/case/%.ref .
    Complex cases  tests/complex/% are compared to tests/complex/%/reference .
   Invidual cases can add a version constraint with [[@@@if %d.%02d]] .
   Complex cases can add a version constraint with a "%/guard" file.
*)

module Version = struct

  type t = {major:int; minor:int}
  let v = Scanf.sscanf Sys.ocaml_version "%d.%d"
      (fun major minor -> { major; minor })

  let v_4_04 = { minor = 4; major = 4 }
  let v_4_06 = { minor = 6; major = 4 }
  let v_4_07 = { minor = 7; major = 4 }
  let v_4_08 = { minor = 8; major = 4 }
  let read s =
    Scanf.sscanf s "%d.%02d" (fun major minor -> {minor;major})
end

let () = Sys.chdir ".."
let pwd = Sys.getcwd ()
let codept =  pwd ^ "/full/codept.exe"
let zip_test = pwd ^ "/tests/step_by_step.exe"
let () = Sys.chdir "../../tests/"



let sep ="−"

let rec skip_num pos s =
  if pos < String.length s then
    match s.[pos] with
    | '0'..'9' -> skip_num (pos+1) s
    | _ -> pos
  else pos

let string_mem s s' pos0 =
let rec mem_string s rel s' pos0 =
  let ls = String.length s in
  rel = ls || begin
    s.[rel] = s'.[pos0]
    && mem_string s (rel+1) s' (pos0 + 1)
  end in
  pos0 + String.length s <= String.length s'
  && mem_string s 0 s' pos0

let diff ppf (x,y) =
  let rec diff ppf x xpos y ypos =
    let stop, xnew, ynew =
      try
        false,
        String.index_from x (xpos+1) '\n',
        String.index_from y (ypos + 1) '\n'
      with Not_found ->
        true, String.length x - 1, String.length y - 1 in
    let maybe_sub s start stop =
      if start > stop then ""
      else String.sub s start stop
    in
    let xs = maybe_sub x (xpos+1) (xnew - xpos -1) in
    let ys = maybe_sub y (ypos+1) (ynew - ypos -1) in
    if (xs <> ys ) then
      Format.printf ">%s@,<%s@," xs ys;
    if stop then () else diff ppf x xnew y ynew in
  diff ppf x (-1) y (-1)

(* Ignore location difference corresponding to the "??" pattern in
   l1.??−end *)
let (%=%) x y =
  let lsx = String.length x in
  let lsy = String.length y in
  let rec check x posx y posy =
    posx = lsx && posy = lsy
    || not (posx = lsx || posy = lsy )
       && (
         if x.[posx] = y.[posy] then
           check x (posx+1) y (posy+1)
         else
           let posx' = skip_num posx x in
           let posy' = skip_num posy y in
           posx' > posx && posy' > posy && posx' < lsx && posy' < lsy
           && string_mem sep x posx'
           && string_mem sep y posy'
           && check x posx' y posy'
       ) in
  check x 0 y 0

type attr =
  | If of Version.t
  | Broken
  | Self_cycle


type variant = {
  name: string;
  cmd: string array option -> string -> string * string array;
  filter: attr -> bool
}

let full_variant =
  let cmd arg x =
    let cmd = match arg with
      | None -> [| "codept"; "-nested"; "-expand-deps"; "-no-alias-deps"; "-deps"; "-k"; x |]
      | Some a -> Array.concat [ [| "codept"|] ; a; [|x|] ] in
    codept, cmd  in
   { name = "full_variant"; cmd; filter = (fun _ -> true) }

let zip_variant =
  let cmd _ x = zip_test, [|"zip_test"; x|] in
  let filter = function Broken | Self_cycle -> false | _ -> true in
  { name="zip"; cmd; filter }

let run arg (v,  (case,_attrs)) =
    let out, inp = Unix.pipe () in
    let pid =
      let main, args = v.cmd arg case in
      Unix.create_process main args Unix.stdin inp inp in
    v.name, case, pid, out

let read_all =
  let len = 1024 in
  let b = Bytes.create len in
  let rec loop f buff =
    let read = Unix.read f b 0 len in
    if read = len then
      (Buffer.add_bytes buff b; loop f buff )
    else
      (Buffer.add_subbytes buff b 0 read; Buffer.contents buff) in
  fun f -> loop f (Buffer.create 17)

let uerror ppf = function
  | Unix.WEXITED x -> Format.fprintf ppf "WEXITED %d" x
  | Unix.WSTOPPED x -> Format.fprintf ppf "WSTOPPED %d" x
  | Unix.WSIGNALED x -> Format.fprintf ppf "WSIGNALED %d" x
let error e x = Format.eprintf "error(%a) with %s@." uerror e x

let refname name =
  Filename.chop_extension name ^ ".ref"

let reference name =
  if Sys.file_exists name then
    let inc = open_in name in
    really_input_string inc (in_channel_length inc)
  else
    ""

let guard f x =
  let guard = x  ^ "/guard" in
  if Sys.file_exists guard then
    let chan = open_in guard in
    let g = input_line chan in
    close_in chan;
    let v = Version.read g in
    if Version.v >= v then
      f x;
  else
    f x

let arg x =
  let arg_file = x ^ "/args" in
  if Sys.file_exists arg_file then
    Some (Arg.read_arg arg_file)
  else
    None

let green ppf s = Format.fprintf ppf "\x1b[32m%s\x1b[0m" s
let red ppf s = Format.fprintf ppf "\x1b[31m%s\x1b[0m" s

let print_arg ppf = function
  | None -> Format.fprintf ppf "None"
  | Some x -> Array.iter (Format.fprintf ppf "%s@ ") x


let dir x =
  let variant = full_variant in
  let arg = arg x in
  let _, _, pid, out = run arg (variant, (x,[])) in
  match snd (Unix.waitpid [] pid) with
  | WEXITED (0|2) ->
    let s = read_all out in
    let ref = reference (x ^ "/reference") in
    if s %=% ref then
      Format.printf "[%s]: %a@." x green "ok"
    else
      Format.printf "[%s]:@ %a,@ args:%a@ %s@,diff:@,@[<v>%a@]@." x red "failure"
        print_arg arg s diff (s,ref)
  | e -> error e x

let extract_attribute x =
  let f = open_in x in
  let rec loop l =
    try
      let nl = input_line f in
      try
        let att = Scanf.sscanf nl "[%@%@%@%s@]" (fun x -> x) in
        loop (att :: l)
      with
      | Scanf.Scan_failure _ -> loop l
    with End_of_file -> l in
  let l = loop [] in
  close_in f;
  l

let parse_if s =
  try
    Scanf.sscanf s "if %d.%02d" (fun x y -> Some(If{major=x;minor=y}))
  with Scanf.Scan_failure _ -> None

let parse_other = function
  | "broken" -> Some Broken
  | "cycle" -> Some Self_cycle
  | _ -> None

let parse l x = match parse_if x with
  | Some x -> x :: l
  | None -> match parse_other x with
    | Some x -> x :: l
    | None -> l

let parse_all x = List.fold_left parse [] @@ extract_attribute x

let if' = function If _ -> true | _ -> false
let find_opt f l = try Some (List.find f l) with Not_found -> None

let filter_case attrs =
  match find_opt if' attrs with
  | Some If v ->
    Version.v >= v
  | _ -> true

let prod x y = List.flatten
    (List.map (fun x -> List.map (fun y -> x,y) y) x)

let cases =
  let variants = [full_variant; zip_variant] in
  let is_source x =
    Filename.check_suffix x "ml" || Filename.check_suffix x "mli" in
  let all =
    List.filter (fun (_,attrs) -> filter_case attrs)
    @@ List.map (fun x -> x, parse_all x )
    @@ List.map (fun x -> "cases/" ^ x )
    @@ List.filter is_source
    @@ Array.to_list @@ Sys.readdir "cases" in
  let all =
    List.filter (fun (v, (_,attrs)) -> List.for_all v.filter attrs)
      @@ prod variants all in
  let post (vname, name, _, x) =
    let s = read_all x in
    let ref = reference (refname name) in
    if s %=% ref then
      Format.printf "[%s]: %a(%s)@." name green "ok" vname
    else begin
      Format.printf "[%s]: %a(%s)@,%s@,diff:@,@[<v>%a@]@." name red "fail" vname
        s diff (s,ref)
    end
  in
  let peek l (_, case, pid, _ as x) =
    match Unix.waitpid [Unix.WNOHANG] pid with
    | 0, _ -> x :: l
    | _, WEXITED (0|2)  -> post x; l
    | _, e  -> error e case; l in
  let rec loop = function
    | [] -> ()
    | pending ->
      loop (List.fold_left peek [] pending) in
  loop (List.map (run None) all)

let () =
  Sys.chdir "complex";
  let all = Sys.readdir "." in
  Array.iter (fun x -> if x <> "." || x <> ".." then
                 guard dir x)
    all
