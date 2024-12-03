let () = match Sys.argv with
  | [| _; day_s; file_name |] -> (match (int_of_string_opt day_s) with
                                  | Some 1 -> let input = Aoc.Day1.read_lists file_name in
                                              Format.printf "Part1: %d\nPart2: %d\n" (Aoc.Day1.part1 input) (Aoc.Day1.part2 input)
                                  | Some 2 -> let input = In_channel.with_open_text file_name In_channel.input_lines in
                                              Format.printf "Part1: %d\n" (Aoc.Day2.part1 input)
                                  | Some 3 -> let concated_input = In_channel.with_open_text file_name (In_channel.fold_lines (^) "") in
                                              Format.printf "Part1: %d\nPart2: %d\n" (Aoc.Day3.part1 concated_input) (Aoc.Day3.part2 concated_input)
                                  | Some _ -> Format.printf "Unknown Day: %s\n" day_s
                                  | None -> Format.printf "Invalid day syntax: %s\n" day_s)
  | _ -> Format.printf "Usage: %s day input_file\n" Sys.argv.(0)


