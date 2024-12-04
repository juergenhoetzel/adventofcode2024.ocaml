open Alcotest


let () =  Alcotest.run "Aoc test suite"
            [
              ("Day1", let testdata = ([1; 2; 3; 3; 3; 4], [3; 3; 3; 4; 5; 9]) in
                       [
                         "Day1 part 1", `Quick, (fun () -> (check int "Expected int" 11 (Aoc.Day1.part1 testdata)));
                         "Day1 part 2", `Quick, (fun () -> (check int "Expected int" 31 (Aoc.Day1.part2 testdata)));
                       ]
              );
              ("Day2", let testdata = ["7 6 4 2 1";"1 2 7 8 9";"9 7 6 2 1";"1 3 2 4 5";"8 6 4 4 1";"1 3 6 7 9"] in
                       ["Day2 part 1", `Quick, (fun () -> (check int "Expected int" 2 (Aoc.Day2.part1 testdata)));]
              );
              ("Day3", let testdata  = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" in
                       [
                         "Day3 part 1", `Quick, (fun () -> (check int "Expected int" 161 (Aoc.Day3.part1 testdata)));
                         "Day3 part 2", `Quick, (fun () -> (check int "Expected int" 48 (Aoc.Day3.part2 testdata)));
              ]);
              ("Day4", let testdata = [|
                             [|'M';'M';'M';'S';'X';'X';'M';'A';'S';'M'|];
                             [|'M';'S';'A';'M';'X';'M';'S';'M';'S';'A'|];
                             [|'A';'M';'X';'S';'X';'M';'A';'A';'M';'M'|];
                             [|'M';'S';'A';'M';'A';'S';'M';'S';'M';'X'|];
                             [|'X';'M';'A';'S';'A';'M';'X';'A';'M';'M'|];
                             [|'X';'X';'A';'M';'M';'X';'X';'A';'M';'A'|];
                             [|'S';'M';'S';'M';'S';'A';'S';'X';'S';'S'|];
                             [|'S';'A';'X';'A';'M';'A';'S';'A';'A';'A'|];
                             [|'M';'A';'M';'M';'M';'X';'M';'M';'M';'M'|];
                             [|'M';'X';'M';'X';'A';'X';'M';'A';'S';'X'|];
                           |] in
                       ["Day4 part 1", `Quick, (fun () -> (check int "Expected int" 18 (Aoc.Day4.part1 testdata)));
                        "Day4 part 2", `Quick, (fun () -> (check int "Expected int" 9 (Aoc.Day4.part2 testdata)));]
                );
            ]
