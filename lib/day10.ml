open! Core
open Poly

type map = { nrows : int; ncols : int; entries : int array array }

let parse_input lines =
  let a = List.map lines ~f:(fun line -> String.to_array line |> Array.map ~f:Char.get_digit_exn) |> Array.of_list in
  { nrows = Array.length a; ncols = Array.length a.(0); entries = a }

let get_directions m (y, x) =
  List.fold
    [ (0, 1); (1, 0); (-1, 0); (0, -1) ]
    ~init:[]
    ~f:(fun acc (yd, xd) ->
      let y2 = y + yd in
      let x2 = x + xd in
      if 0 <= y2 && y2 < m.nrows && 0 <= x2 && x2 < m.ncols then (y2, x2) :: acc else acc)

let get_trailheads m = Array.concat_mapi m.entries ~f:(fun y a -> Array.filter_mapi a ~f:(fun x i -> if i = 0 then Some (y, x) else None))

let get_paths m pos =
  let queue = [ [ pos ] ] in
  let valid_paths = [] in
  let rec loop queue valid_paths =
    match queue with
    | [] -> valid_paths
    | path :: queue -> (
        let y, x = List.hd_exn path in
        match m.entries.(y).(x) with
        | 9 -> loop queue (path :: valid_paths)
        | height ->
            let new_pos = List.filter (get_directions m (y, x)) ~f:(fun (y2, x2) -> m.entries.(y2).(x2) = height + 1) in
            let new_paths = List.map ~f:(fun pos -> pos :: path) new_pos in
            loop (List.append new_paths queue) valid_paths)
  in
  loop queue valid_paths

let part1 m =
  let ths = get_trailheads m in
  Array.map ths ~f:(fun pos -> get_paths m pos |> List.map ~f:List.hd_exn |> Set.of_list (module Tuple.Comparator (Int) (Int)) |> Set.length)
  |> Array.reduce_exn ~f:( + )
