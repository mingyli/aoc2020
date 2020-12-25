open Base

type orientation = North | South | East | West

let turn_left = function
  | North -> West
  | South -> East
  | East -> North
  | West -> South

type action =
  | MoveNorth of int
  | MoveSouth of int
  | MoveEast of int
  | MoveWest of int
  | TurnLeft of int
  | TurnRight of int
  | MoveForward of int

let action_of_string s =
  let pattern =
    Re.seq [ Re.group (Re.set "NSEWLRF"); Re.group (Re.rep Re.digit) ]
    |> Re.compile
  in
  let groups = Re.exec pattern s in
  let value = Re.Group.get groups 2 |> Int.of_string in
  match Re.Group.get groups 1 with
  | "N" -> MoveNorth value
  | "S" -> MoveSouth value
  | "E" -> MoveEast value
  | "W" -> MoveWest value
  | "L" -> TurnLeft value
  | "R" -> TurnRight value
  | "F" -> MoveForward value
  | _ -> assert false

let rec apply_action action state =
  let x, y, orientation = state in
  match action with
  | MoveNorth v -> (x, y + v, orientation)
  | MoveSouth v -> (x, y - v, orientation)
  | MoveEast v -> (x + v, y, orientation)
  | MoveWest v -> (x - v, y, orientation)
  | TurnLeft degrees ->
      let orientation =
        match degrees with
        | 90 -> turn_left orientation
        | 180 -> orientation |> turn_left |> turn_left
        | 270 -> orientation |> turn_left |> turn_left |> turn_left
        | _ -> assert false
      in
      (x, y, orientation)
  | TurnRight degrees ->
      let orientation =
        match degrees with
        | 90 -> orientation |> turn_left |> turn_left |> turn_left
        | 180 -> orientation |> turn_left |> turn_left
        | 270 -> orientation |> turn_left
        | _ -> assert false
      in
      (x, y, orientation)
  | MoveForward v -> (
      match orientation with
      | North -> apply_action (MoveNorth v) state
      | South -> apply_action (MoveSouth v) state
      | East -> apply_action (MoveEast v) state
      | West -> apply_action (MoveWest v) state )

let read_actions () =
  let open Stdio in
  let file = In_channel.create "input.txt" in
  In_channel.input_lines file |> List.map ~f:action_of_string

let day12a actions =
  actions
  |> List.fold ~init:(0, 0, East) ~f:(fun state action ->
         apply_action action state)
  |> fun (x, y, _) -> Int.abs x + Int.abs y

let rotate_left (x, y) = (-y, x)

let apply_action action (ship, waypoint) =
  let x, y = ship in
  let dx, dy = waypoint in
  match action with
  | MoveNorth v -> (ship, (dx, dy + v))
  | MoveSouth v -> (ship, (dx, dy - v))
  | MoveEast v -> (ship, (dx + v, dy))
  | MoveWest v -> (ship, (dx - v, dy))
  | TurnLeft degrees ->
      let waypoint =
        match degrees with
        | 90 -> waypoint |> rotate_left
        | 180 -> waypoint |> rotate_left |> rotate_left
        | 270 -> waypoint |> rotate_left |> rotate_left |> rotate_left
        | _ -> assert false
      in
      (ship, waypoint)
  | TurnRight degrees ->
      let waypoint =
        match degrees with
        | 90 -> waypoint |> rotate_left |> rotate_left |> rotate_left
        | 180 -> waypoint |> rotate_left |> rotate_left
        | 270 -> waypoint |> rotate_left
        | _ -> assert false
      in
      (ship, waypoint)
  | MoveForward times -> ((x + (dx * times), y + (dy * times)), waypoint)

let day12b actions =
  actions
  |> List.fold
       ~init:((0, 0), (10, 1))
       ~f:(fun state action -> apply_action action state)
  |> fun ((x, y), _) -> Int.abs x + Int.abs y

let () =
  let actions = read_actions () in
  day12b actions |> Stdio.printf "%d\n"
