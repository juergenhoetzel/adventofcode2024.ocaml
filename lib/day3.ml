type instruction = Do|Dont|Mul of int * int
let s = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
let s2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
let inst_r = Str.regexp {|\(do()\|don't()\|mul(\([0-9]+\),\([0-9]+\))\)|}
let rec get_all_matches s i =
  match Str.search_forward inst_r s i with
  | j  -> let instruction =
            let s0 = (Str.matched_group 0 s) in
            if s0 = "do()" then Do
            else if s0 = "don't()" then Dont
            else let a1 = Str.matched_group 2 s|>int_of_string in
                 let a2 = Str.matched_group 3 s|>int_of_string in
                 Mul(a1,a2)
          in instruction::get_all_matches s (j + 1)

  | exception Not_found -> []


let part1 s =
  get_all_matches s 0|>List.fold_left (fun acc inst -> match inst with
                                                       | Mul(x, y) -> acc + x * y
                                                       | _ -> acc) 0

let part2 s =
  get_all_matches s 0|>List.fold_left (fun (m,acc) inst -> match inst with
                                                       | Mul(x, y) -> m,(acc + x * y*m)
                                                       | Dont -> 0, acc
                                                       | Do -> 1, acc) (1,0) |> snd


