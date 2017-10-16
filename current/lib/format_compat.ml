let transform fmt f =
  { fmt with
    Format.out_newline = (fun () -> ());
    out_spaces = (fun _ -> ());
    out_indent = (fun _ -> ());
    out_string = f
  }
