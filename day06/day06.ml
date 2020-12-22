open Base
open Stdio

let read_groups () =
  let file = In_channel.create "input.txt" in
  In_channel.input_lines file
  |> List.group ~break:(fun _ line -> String.equal line "")
  |> List.map ~f:(List.filter ~f:(fun s -> not (String.is_empty s)))

let day06a groups =
  groups |> List.map ~f:String.concat
  |> List.map ~f:(fun s -> List.init (String.length s) ~f:(String.get s))
  |> List.map ~f:(List.fold ~f:Set.add ~init:(Set.empty (module Char)))
  |> List.map ~f:Set.length |> List.fold ~init:0 ~f:( + )

let day06b groups =
  let num_universal_answers group =
    group
    |> List.map ~f:(fun s -> List.init (String.length s) ~f:(String.get s))
    |> List.map ~f:(List.fold ~f:Set.add ~init:(Set.empty (module Char)))
    |> fun l -> List.fold l ~f:Set.inter ~init:(List.hd_exn l) |> Set.length
  in
  groups |> List.map ~f:num_universal_answers |> List.fold ~init:0 ~f:( + )

let () =
  let groups = read_groups () in
  day06b groups |> printf "%d\n"
