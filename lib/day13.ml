open! Core

type machine = { a0 : int; a1 : int; b0 : int; b1 : int; r0 : int; r1 : int }

let parse_machines s =
  let digits = Re2.create_exn "[0-9]+" in
  let xs = Re2.find_all_exn digits s |> List.map ~f:Int.of_string in
  let rec loop machines = function a0 :: a1 :: b0 :: b1 :: r0 :: r1 :: xs -> loop ({ a0; a1; b0; b1; r0; r1 } :: machines) xs | _ -> machines in
  loop [] xs

let gauss machine =
  { machine with a1 = 0; b1 = (machine.b1 * machine.a0) - (machine.b0 * machine.a1); r1 = (machine.r1 * machine.a0) - (machine.r0 * machine.a1) }

let guess_buttons machine =
  match gauss machine with
  | { a0; a1 = 0; b0; b1; r0; r1 } -> (
      match Int.rem r1 b1 with
      | 0 ->
          let b = r1 / b1 in
          let n = r0 - (b * b0) in
          if Int.rem n a0 = 0 then Some (n / a0, b) else None
      | _ -> None)
  | _ -> None

let tweak machine = { machine with r0 = machine.r0 + 10000000000000; r1 = machine.r1 + 10000000000000 }

let buttons_score machines =
  List.map ~f:guess_buttons machines |> List.fold ~init:0 ~f:(fun acc bs -> match bs with Some (b1, b2) -> (b1 * 3) + b2 + acc | None -> acc)
