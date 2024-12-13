open! Core

let blink = function
  | 0 -> [ 1 ]
  | x -> (
      let s_len = string_of_int x |> String.length in
      match s_len mod 2 with
      | 0 ->
          let s = Int.to_string x in
          let left, right = (String.sub s ~pos:0 ~len:(s_len / 2), String.sub s ~pos:(s_len / 2) ~len:(s_len / 2)) in
          [ Int.of_string left; Int.of_string right ]
      | _ -> [ x * 2024 ])

let count_add m key x = Map.change m key ~f:(fun v -> match v with Some y -> Some (y + x) | None -> Some x)

let update_work m x count =
  match blink x with
  | [ l; r ] ->
      let lm = count_add m l count in
      count_add lm r count
  | [ y ] -> count_add m y count
  | _ -> failwith "Invalid blink"

let blink_sum xs n =
  let work = List.fold xs ~init:(Map.empty (module Int)) ~f:(fun m x -> Map.change m x ~f:(fun v -> match v with Some y -> Some (y + 1) | None -> Some 1)) in
  let rec loop work n =
    match n with
    | 0 -> Map.fold work ~f:(fun ~key:_ ~data:v sum -> sum + v) ~init:0
    | n ->
        let new_work = Map.fold work ~init:(Map.empty (module Int)) ~f:(fun ~key:k ~data:v m -> update_work m k v) in
        loop new_work (n - 1)
  in
  loop work n

let parse_line line = Str.split (Str.regexp "[ \n\r\x0c\t]+") line |> List.map ~f:Int.of_string
let part1 xs = blink_sum xs 25
let part2 xs = blink_sum xs 75
