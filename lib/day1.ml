module IntMap = Map.Make(Int)

type input = int list * int list

let read_lists file_name =
  let xs1, xs2 = In_channel.with_open_text file_name (In_channel.fold_lines (fun (xs1, xs2) line -> match Str.split (Str.regexp "[\t ]+") line with
                                                                                         | [x1; x2] -> int_of_string x1::xs1,int_of_string x2::xs2
                                                                                         | _ -> xs1,xs2
                                                        ) ([],[])) in
  xs1, xs2

let part1 (xs1,xs2) =
  List.fold_left2 (fun acc x1 x2 ->  (abs (x1 - x2))  + acc) 0   (List.sort compare xs1) (List.sort compare xs2)

let part2 (xs1,xs2) =
  let counts = List.fold_left (fun counts x -> match IntMap.find_opt x counts with
                                               | None -> IntMap.add x 1 counts
                                               | Some n -> IntMap.add x (n + 1) counts
                 ) IntMap.empty xs2 in
  List.fold_left (fun acc x -> match IntMap.find_opt x counts with
                     | None -> acc
                     | Some n -> x*n + acc) 0  xs1
