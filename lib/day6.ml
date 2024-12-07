open! Core
open Poly

type player_map = { locations : (int * int) list; start_pos : int * int; n_rows : int; n_cols : int }

let parse_input lines =
  let locations, start_pos =
    Base.List.foldi lines
      ~f:(fun y (locations, start_pos) line ->
        let new_locations = String.foldi line ~f:(fun x locations c -> if Char.equal c '#' then (y, x) :: locations else locations) ~init:[] in
        let start_pos_new = match String.index line '^' with Some x -> Some (y, x) | None -> start_pos in
        (List.append locations new_locations, start_pos_new))
      ~init:([], None)
  in
  match start_pos with
  | Some pos -> { locations; start_pos = pos; n_rows = List.length lines; n_cols = String.length (List.hd_exn lines) }
  | None -> failwith "Start position not found"

let is_in_field map (y, x) = 0 <= y && y < map.n_rows && 0 <= x && x < map.n_cols
let turn = function -1, 0 -> (0, 1) | 0, 1 -> (1, 0) | 1, 0 -> (0, -1) | 0, -1 -> (-1, 0) | _ -> failwith "Invalid argument"

let walk map =
  let pos = map.start_pos in
  let rec loop visited (y, x) (yd, xd) =
    let new_visited = ((y, x), (yd, xd)) :: visited in
    let new_pos = (y + yd, x + xd) in
    if not (is_in_field map new_pos) then Some new_visited
    else if List.exists map.locations ~f:(( = ) new_pos) then
      let new_direction = turn (yd, xd) in
      loop visited (y, x) new_direction
    else if List.exists ~f:(( = ) (new_pos, (yd, xd))) visited then None
    else loop new_visited new_pos (yd, xd)
  in
  loop [] pos (-1, 0)

let part1 map =
  match walk map with
  | Some movements -> List.map ~f:fst movements |> Set.of_list (module Tuple.Comparator (Int) (Int)) |> Set.length
  | _ -> failwith "Loop detected"

let part2 map =
  match walk map with
  | None -> failwith "Loop detected"
  | Some movements -> 
     List.fold ~f:(fun acc (pos, _) ->
         match walk {locations= pos::map.locations; n_rows = map.n_rows; n_cols = map.n_cols; start_pos = map.start_pos} with
         | None -> (Set.add acc pos)
         | Some _ -> acc
       )  ~init:(Set.empty (module Tuple.Comparator (Int) (Int)))  movements
  |> Set.length



