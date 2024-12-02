open Alcotest



let day1_input = ([1; 2; 3; 3; 3; 4], [3; 3; 3; 4; 5; 9])

let day1_suite =
  [
    "Day1 part 1", `Quick, (fun () -> (check int "Expected int" 11 (Aoc.Day1.part1 day1_input)));
    "Day1 part 2", `Quick, (fun () -> (check int "Expected int" 31 (Aoc.Day1.part2 day1_input)));
  ]

let () =  Alcotest.run "Aoc test suite"
[
  ("Day1", day1_suite);
]
