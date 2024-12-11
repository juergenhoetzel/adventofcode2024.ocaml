let () =
  match Sys.argv with
  | [| _; day_s; file_name |] -> (
      match int_of_string_opt day_s with
      | Some 1 ->
          let input = Aoc.Day1.read_lists file_name in
          Format.printf "Part1: %d\nPart2: %d\n" (Aoc.Day1.part1 input) (Aoc.Day1.part2 input)
      | Some 2 ->
          let input = In_channel.with_open_text file_name In_channel.input_lines in
          Format.printf "Part1: %d\n" (Aoc.Day2.part1 input)
      | Some 3 ->
          let concated_input = In_channel.with_open_text file_name (In_channel.fold_lines ( ^ ) "") in
          Format.printf "Part1: %d\nPart2: %d\n" (Aoc.Day3.part1 concated_input) (Aoc.Day3.part2 concated_input)
      | Some 4 ->
          let input = In_channel.with_open_text file_name In_channel.input_lines |> List.map Aoc.Day4.parse_line |> Array.of_list in
          Format.printf "Part1: %d\nPart2: %d\n" (Aoc.Day4.part1 input) (Aoc.Day4.part2 input)
      | Some 5 ->
          let order_map, updates = Aoc.Day5.parse_file file_name in
          Format.printf "Part1: %d\n" (Aoc.Day5.part1 order_map updates)
      | Some 6 ->
          let lines = In_channel.with_open_text file_name In_channel.input_lines in
          let player_map = Aoc.Day6.parse_input lines in
          Format.printf "Part1: %d\n" (Aoc.Day6.part1 player_map);
          Format.printf "Part2: %d\n" (Aoc.Day6.part2 player_map)
      | Some 10 ->
          let lines = In_channel.with_open_text file_name In_channel.input_lines in
          let m = Aoc.Day10.parse_input lines in
          Format.printf "Part1: %d\n" (Aoc.Day10.part1 m);
          Format.printf "Part2: %d\n" (Aoc.Day10.part2 m)
      | Some 11 -> (
          match In_channel.with_open_text file_name In_channel.input_line with
          | Some line ->
              let xs = Aoc.Day11.parse_line line in
              Format.printf "Part1: %d\n" (Aoc.Day11.part1 xs);
              Format.printf "Part2: %d\n" (Aoc.Day11.part2 xs)
          | _ -> failwith "Missing input")
      | Some _ -> Format.printf "Unknown Day: %s\n" day_s
      | None -> Format.printf "Invalid day syntax: %s\n" day_s)
  | _ -> Format.printf "Usage: %s day input_file\n" Sys.argv.(0)
