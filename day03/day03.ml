open Base
open Stdio

let read_grid () =
  let file = In_channel.create "input.txt" in
  In_channel.input_lines file
  |> List.map ~f:(fun line ->
         Array.init (String.length line) ~f:(String.get line))
  |> Array.of_list

let toboggan grid (dr, dc) =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let rec aux row col acc =
    if row >= height then acc
    else
      aux (row + dr)
        ((col + dc) % width)
        (if Char.equal grid.(row).(col) '#' then acc + 1 else acc)
  in
  aux 0 0 0

let day3a grid = toboggan grid (1, 3)

let day3b grid =
  let vectors = [ (1, 1); (1, 3); (1, 5); (1, 7); (2, 1) ] in
  List.map vectors ~f:(toboggan grid) |> List.reduce_exn ~f:( * )

let () =
  let grid = read_grid () in
  day3b grid |> printf "%d\n"
