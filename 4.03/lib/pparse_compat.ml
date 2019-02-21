let stderr = Format.err_formatter

let implementation input =
  Pparse.parse_implementation stderr ~tool_name:"codept" input

let interface input =
  Pparse.parse_interface stderr ~tool_name:"codept" input
