(* Xavier Leroy & Damien Doligez, INRIA Rocquencourt (c) 1996 *)

let is_dir_sep_win32 s i = let c = s.[i] in c = '/' || c = '\\' || c = ':'
let is_dir_sep_unix s i = s.[i] = '/'
let is_dir_sep = match Sys.os_type with
  | "Win32" -> is_dir_sep_win32
  | _ -> is_dir_sep_unix

let extension_len name =
  let rec check i0 i =
    if i < 0 || is_dir_sep name i then 0
    else if name.[i] = '.' then check i0 (i - 1)
    else String.length name - i0 in
  let rec search_dot i =
    if i < 0 || is_dir_sep name i then 0
    else if name.[i] = '.' then check i (i - 1)
    else search_dot (i - 1) in
  search_dot (String.length name - 1)

let extension name =
  let l = extension_len name in
  if l = 0 then "" else
  let l = l - 1 in
  String.sub name (String.length name - l) l

let remove_extension name =
  let l = extension_len name in
  if l = 0 then name else
  String.sub name 0 (String.length name - l)

let split_on_char sep s =
  let sub start stop =
    String.sub s start (stop-start) in
  let rec split l last pos =
    if pos = 0 then
      if s.[pos] = sep then
        "" :: sub (pos+1) last :: l
      else
        sub pos last :: l
    else if s.[pos] = sep then
      split (sub (pos+1) last :: l) pos (pos-1)
    else
      split l last (pos-1) in
  let n = String.length s in
  split [] n (n-1)

let opt conv s = try Some(conv s) with Failure _ -> None

let filter_map f l =
   List.rev @@ List.fold_left (fun acc x ->
      match f x with
      | Some x -> x :: acc
      | None -> acc
    ) [] l
