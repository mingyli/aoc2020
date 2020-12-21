open Base
open Stdio

let read_boarding_passes () =
  let file = In_channel.create "input.txt" in
  In_channel.input_lines file
  |> List.map ~f:(fun s ->
         (String.sub s ~pos:0 ~len:7, String.sub s ~pos:7 ~len:3))

let row seq =
  List.fold seq ~init:(0, 128) ~f:(fun (low, high) ch ->
      let mid = (low + high) / 2 in
      match ch with 'F' -> (low, mid) | _ -> (mid, high))

let column seq =
  List.fold seq ~init:(0, 8) ~f:(fun (low, high) ch ->
      let mid = (low + high) / 2 in
      match ch with 'L' -> (low, mid) | _ -> (mid, high))

let seat_number boarding_pass =
  let rows, columns = boarding_pass in
  let row, _ = row (String.to_list rows) in
  let col, _ = column (String.to_list columns) in
  (row * 8) + col

let day05a boarding_passes =
  boarding_passes |> List.map ~f:seat_number
  |>
  let rec max_val = function
    | [] -> assert false
    | [ head ] -> head
    | head :: tail -> max head (max_val tail)
  in
  max_val

let day05b boarding_passes =
  let all_seats =
    List.fold
      (List.init 1024 ~f:(fun i -> i))
      ~f:Set.add
      ~init:(Set.empty (module Int))
  in
  let occupied = boarding_passes |> List.map ~f:seat_number in
  let seats = List.fold occupied ~f:Set.remove ~init:all_seats in
  let rec find_isolated_seat = function
    | first :: second :: third :: tail ->
        if second - first > 1 && third - second > 1 then second
        else find_isolated_seat (second :: third :: tail)
    | _ -> assert false
  in
  find_isolated_seat (Set.elements seats)

let () =
  let boarding_passes = read_boarding_passes () in
  day05b boarding_passes |> printf "%d\n"
