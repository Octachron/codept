
let () = Sys.chdir ".."
let pwd = Sys.getcwd ()
let codept =  pwd ^ "/full/codept.exe"
let () = Printf.printf "codept: %s\n" codept
let () = Sys.chdir "../../tests/"

let run case =
  let out, inp = Unix.pipe () in
  let pid = Unix.create_process
      codept [| "codept"; "-no-alias-deps"; "-deps"; "-k"; case |]
      Unix.stdin inp inp in
  case, pid, out

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

let error x = Format.eprintf "error with %s@." x

let reference name =
  let name = Filename.chop_extension name ^ ".ref" in
  if Sys.file_exists name then
    let inc = open_in name in
    really_input_string inc (in_channel_length inc)
  else
    ""

let diff ppf (x,y) =
  let rec diff ppf x xpos y ypos =
    let stop, xnew, ynew =
      try
        false,
        String.index_from x (xpos+1) '\n',
        String.index_from y (ypos + 1) '\n'
      with Not_found ->
        true, String.length x - 1, String.length y - 1 in
    let xs = String.sub x (xpos+1) (xnew - xpos) in
    let ys = String.sub y (ypos+1) (ynew - ypos) in
    if (xs <> ys ) then
      Format.printf ">%s<%s@," xs ys;
    if stop then () else diff ppf x xnew y ynew in
  diff ppf x 0 y 0


let cases =
  let is_source x =
    Filename.check_suffix x "ml" || Filename.check_suffix x "mli" in
  let all =
    List.map (fun x -> "cases/" ^ x )
    @@ List.filter is_source
    @@ Array.to_list @@ Sys.readdir "cases" in
  let post (name, _, x) =
    let s = read_all x in
    let ref = reference name in
    if s = ref then
      Format.printf "[%s]: ok@." name
    else
      Format.printf "[%s]:@ @[<v>%a@]@." name diff (s,ref)
  in
  let peek l (case, pid, _ as x) =
    match Unix.waitpid [Unix.WNOHANG] pid with
    | 0, _ -> x :: l
    | _, WEXITED (0|2)  -> post x; l
    | _ -> error case; l in
  let rec loop = function
    | [] -> ()
    | pending ->
      loop (List.fold_left peek [] pending) in
  loop (List.map run all)

