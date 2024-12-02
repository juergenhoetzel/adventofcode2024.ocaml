let () = match Sys.argv with
  | [| _; day_s; file_name |] -> (match (int_of_string_opt day_s) with
                                  | Some 1 -> let input = Aoc.Day1.read_lists file_name in
                                              Format.printf "Part1: %d\nPart2: %d\n" (Aoc.Day1.part1 input) (Aoc.Day1.part2 input)
                                  | Some _ -> Format.printf "Unknown Day: %s" day_s
                                  | None -> Format.printf "Invalid day syntax: %s" day_s)
  | _ -> Format.printf "Usage: %s day input_file\n" Sys.argv.(0)


