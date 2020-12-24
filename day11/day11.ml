open Base

let read_grid () =
  let open Stdio in
  let file = In_channel.create "input.txt" in
  In_channel.input_lines file
  |> List.map ~f:(fun line ->
         Array.init (String.length line) ~f:(String.get line))
  |> Array.of_list

type seat = Floor | Empty | Occupied | EmptyToOccupied | OccupiedToEmpty

let seat_of_char ch =
  match ch with
  | '.' -> Floor
  | 'L' -> Empty
  | '#' -> Occupied
  | _ -> assert false

let num_occupied_seats =
  List.count ~f:(function Occupied | EmptyToOccupied -> true | _ -> false)

let num_changes grid =
  grid
  |> Array.map
       ~f:
         (Array.count ~f:(function
           | OccupiedToEmpty | EmptyToOccupied -> true
           | _ -> false))
  |> Array.sum (module Int) ~f:Fn.id

let resolve_grid =
  Array.map
    ~f:
      (Array.map ~f:(function
        | EmptyToOccupied -> Occupied
        | OccupiedToEmpty -> Empty
        | seat -> seat))

let transition grid =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let in_bounds (r, c) = 0 <= r && r < height && 0 <= c && c < width in
  let grid =
    Array.mapi grid ~f:(fun r row ->
        Array.mapi row ~f:(fun c seat ->
            let neighbors =
              [
                (r - 1, c - 1);
                (r - 1, c);
                (r - 1, c + 1);
                (r, c - 1);
                (r, c + 1);
                (r + 1, c - 1);
                (r + 1, c);
                (r + 1, c + 1);
              ]
              |> List.filter_map ~f:(fun (r, c) ->
                     if in_bounds (r, c) then Some grid.(r).(c) else None)
            in
            let num_occupied_neighbors = num_occupied_seats neighbors in
            match seat with
            | Empty when num_occupied_neighbors = 0 -> EmptyToOccupied
            | Occupied when num_occupied_neighbors >= 4 -> OccupiedToEmpty
            | seat -> seat))
  in
  let num_changes = num_changes grid in
  let grid = resolve_grid grid in
  (grid, num_changes)

let count_occupied_seats (grid : seat array array) =
  grid
  |> Array.map ~f:(Array.count ~f:(function Occupied -> true | _ -> false))
  |> Array.sum (module Int) ~f:Fn.id

let day11a (grid : char array array) =
  let grid = grid |> Array.map ~f:(Array.map ~f:seat_of_char) in
  List.init 999 ~f:Fn.id
  |> List.fold_until ~init:grid
       ~f:(fun grid _ ->
         let grid, num_changes = transition grid in
         if num_changes = 0 then Stop (count_occupied_seats grid)
         else Continue grid)
       ~finish:(fun _ -> assert false)

let transition grid =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let in_bounds (r, c) = 0 <= r && r < height && 0 <= c && c < width in
  let rec slide (r, c) (dr, dc) =
    if not (in_bounds (r, c)) then None
    else
      let seat = grid.(r).(c) in
      match seat with
      | Floor -> slide (r + dr, c + dc) (dr, dc)
      | seat -> Some seat
  in
  let grid =
    Array.mapi grid ~f:(fun r row ->
        Array.mapi row ~f:(fun c seat ->
            let neighbors =
              let vectors =
                [
                  (-1, -1);
                  (-1, 0);
                  (-1, 1);
                  (0, -1);
                  (0, 1);
                  (1, -1);
                  (1, 0);
                  (1, 1);
                ]
              in
              vectors
              |> List.filter_map ~f:(fun (dr, dc) ->
                     slide (r + dr, c + dc) (dr, dc))
            in
            let num_occupied_neighbors = num_occupied_seats neighbors in
            match seat with
            | Empty when num_occupied_neighbors = 0 -> EmptyToOccupied
            | Occupied when num_occupied_neighbors >= 5 -> OccupiedToEmpty
            | seat -> seat))
  in
  let num_changes = num_changes grid in
  let grid = resolve_grid grid in
  (grid, num_changes)

let day11b grid =
  let grid = grid |> Array.map ~f:(Array.map ~f:seat_of_char) in
  List.init 9999 ~f:Fn.id
  |> List.fold_until ~init:grid
       ~f:(fun grid _ ->
         let grid, num_changes = transition grid in
         if num_changes = 0 then Stop (count_occupied_seats grid)
         else Continue grid)
       ~finish:(fun _ -> assert false)

let () =
  let grid = read_grid () in
  day11b grid |> Stdio.printf "%d\n"
