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

let opt conv s = try Some(conv s) with Failure _ -> None

let filter_map f l =
   List.rev @@ List.fold_left (fun acc x ->
      match f x with
      | Some x -> x :: acc
      | None -> acc
    ) [] l

(* Copyright (c) 2016 The astring programmers *)

let add ~empty s ~start ~stop acc =
  if start = stop then ( if not empty then acc else "" :: acc )
  else String.sub s start (stop - start) :: acc

let cuts ~empty ~sep s =
  let sep_len = String.length sep in
  if sep_len = 0 then invalid_arg "cuts: the separator is empty" ;
  let s_len = String.length s in
  let max_sep_idx = sep_len - 1 in
  let max_s_idx = s_len - sep_len in
  let rec check_sep start i k acc =
    if k > max_sep_idx then
      let new_start = i + sep_len in
      scan new_start new_start (add ~empty s ~start ~stop:i acc)
    else
      if s.[i + k] = sep.[k]
      then check_sep start i (k + 1) acc
      else scan start (i + 1) acc
  and scan start i acc =
    if i > max_s_idx then
      if start = 0 then ( if not empty && s_len = 0 then [] else [s])
      else List.rev (add ~empty s ~start ~stop:s_len acc)
    else
      if s.[i] = sep.[0]
      then check_sep start i 1 acc
      else scan start (i + 1) acc in
  scan 0 0 []
