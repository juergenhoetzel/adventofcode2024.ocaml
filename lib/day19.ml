open! Core

let parse_input s =
  match String.split_lines s with twls :: _ :: patterns -> (String.split ~on:',' twls |> List.map ~f:String.strip, patterns) | _ -> failwith "Invalid input"

let combinations prefixes design =
  let cache = ref (Map.of_alist_exn (module String) [ ("", 1) ]) in
  (* Poor man's caching  *)
  let rec loop cache s =
    match Map.find !cache s with
    | Some i -> i
    | None ->
        let result =
          List.map prefixes ~f:(fun prefix -> match String.chop_prefix_exn s ~prefix with exception Invalid_argument _ -> 0 | ss -> loop cache ss)
          |> List.fold ~init:0 ~f:( + )
        in
        cache := Map.add_exn !cache ~key:s ~data:result;
        result
  in

  loop cache design

let part1 prefixes designs = List.count designs ~f:(fun design -> combinations prefixes design > 0)
let part2 prefixes designs = List.fold designs ~init:0 ~f:(fun acc design -> acc + combinations prefixes design)
