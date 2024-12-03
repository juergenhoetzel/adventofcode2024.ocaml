open Alcotest


let () =  Alcotest.run "Aoc test suite"
            [
              ("Day1", let testdata = ([1; 2; 3; 3; 3; 4], [3; 3; 3; 4; 5; 9]) in
                       [
                         "Day1 part 1", `Quick, (fun () -> (check int "Expected int" 11 (Aoc.Day1.part1 testdata)));
                         "Day1 part 2", `Quick, (fun () -> (check int "Expected int" 31 (Aoc.Day1.part2 testdata)));
                       ]
              );
              ("Day2", let testdata  = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" in
                       [
                         "Day3 part 1", `Quick, (fun () -> (check int "Expected int" 161 (Aoc.Day3.part1 testdata)));
                         "Day3 part 2", `Quick, (fun () -> (check int "Expected int" 48 (Aoc.Day3.part2 testdata)));
              ]);
            ]
