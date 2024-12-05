open! Base

let is_valid_order order_map updates =
  let rec loop seen = function
    | [] -> true
    | x :: ys -> (
        match Map.find order_map x with
        | Some befores when Set.is_empty (Set.inter befores seen) -> loop (Set.add seen x) ys
        | Some _ -> false
        | None -> loop (Set.add seen x) ys)
  in
  loop (Set.empty (module Int)) updates

let part1 order_map updates =
  List.filter updates ~f:(fun update -> is_valid_order order_map update)
  |> List.fold_left ~init:0 ~f:(fun acc update ->
         let i = List.length update / 2 in
         List.nth_exn update i + acc)

let parse_file file_name =
  let parts = In_channel.with_open_text file_name In_channel.input_all |> Str.split (Str.regexp "^$\n") in
  let order_map =
    List.nth_exn parts 0 |> String.split_lines
    |> List.map ~f:(fun line ->
           let xs = String.split line ~on:'|' in
           (Int.of_string (List.nth_exn xs 0), Int.of_string (List.nth_exn xs 1)))
    |> Map.of_alist_fold (module Int) ~f:(fun acc x -> Set.add acc x) ~init:(Set.empty (module Int))
  in
  let updates = List.nth_exn parts 1 |> String.split_lines |> List.map ~f:(fun line -> String.split line ~on:',' |> List.map ~f:Int.of_string) in
  (order_map, updates)
