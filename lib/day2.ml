let in_between min max =
  let rec loop prev = function
    | [] -> true
    | x::xs -> match prev with
               | Some p when (x-p) < min || (x-p) > max -> false
               | Some _ |None -> loop (Some x) xs
  in loop None

let parse_lines = List.map (fun line -> Str.split (Str.regexp "[ \n\r\x0c\t]+") line|>List.map int_of_string)

let is_safe line = in_between (-3) (-1) line || in_between 1 3 line

let part1 lines = parse_lines lines |> List.map is_safe|> List.filter Fun.id|>List.length
