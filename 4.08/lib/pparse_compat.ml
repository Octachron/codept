let _stderr = Format.err_formatter

let implementation input =
  Pparse.parse_implementation ~tool_name:"codept" input

let interface input =
  Pparse.parse_interface ~tool_name:"codept" input
