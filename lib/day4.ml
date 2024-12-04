let count_xmas rows y x =
  let directions = [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)] in
  List.map (fun (x_diff,y_diff) ->
      List.fold_left (fun acc i  -> let y2 = y + y_diff*i in
                                    let x2 = x + x_diff*i in
                                    try rows.(y2).(x2)::acc with Invalid_argument _ -> acc)
        [] [0;1;2;3]
    ) directions |> List.filter ((=) ['S'; 'A'; 'M'; 'X']) |> List.length



let part1 rows =
  Array.mapi (fun y row-> Array.mapi (fun x _ -> count_xmas rows y x) row |> Array.fold_left (+) 0) rows |> Array.fold_left (+) 0

let is_x_mas rows y x =  try let d1 = (rows.(y).(x),rows.(y+1).(x+1),rows.(y+2).(x+2)) in
                         let d2 = (rows.(y).(x+2),rows.(y+1).(x+1),rows.(y+2).(x)) in
                         (d1 = ('M','A','S') || d1 = ('S','A','M')) && (d2 = ('M','A','S') || d2 = ('S','A','M')) with Invalid_argument _ -> false

let part2 rows =
  Array.mapi (fun y row-> Array.mapi (fun x _ -> if is_x_mas rows y x then 1 else 0) row |> Array.fold_left (+) 0) rows |> Array.fold_left (+) 0

let parse_line line =  Array.init (String.length line) (String.get line)
