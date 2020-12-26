open Base

let rule_from_string s =
  let rule =
    Re.seq
      [
        Re.group (Re.rep Re.digit);
        Re.str " ";
        Re.group (Re.rep Re.notnl);
        Re.alt [ Re.str " bag"; Re.str " bags" ];
      ]
  in
  let pattern =
    Re.seq
      [
        Re.group (Re.rep Re.notnl);
        Re.str " bags contain ";
        Re.alt
          [
            Re.str "no other bags";
            Re.group (Re.seq [ rule; Re.rep (Re.seq [ Re.str ", "; rule ]) ]);
          ];
        Re.str ".";
      ]
    |> Re.compile
  in
  let groups = Re.exec pattern s in
  let container = Re.Group.get groups 1 in
  let rules =
    if Re.Group.test groups 2 then
      let rule_pattern = Re.compile rule in
      let rules = Re.Group.get groups 2 in
      Re.split (Re.str ", " |> Re.compile) rules
      |> List.map ~f:(fun rule ->
             let groups = Re.exec rule_pattern rule in
             let value = Re.Group.get groups 1 |> Int.of_string in
             let bag = Re.Group.get groups 2 in
             (value, bag))
    else []
  in
  (container, rules)

let rec find_containing_bags rules target =
  let containing_bags =
    Hashtbl.filteri rules ~f:(fun ~key:_ ~data ->
        List.exists data ~f:(fun (_value, bag) -> String.equal bag target))
    |> Hashtbl.keys
    |> Hash_set.of_list (module String)
  in
  let more_bags =
    containing_bags |> Hash_set.to_list
    |> List.map ~f:(find_containing_bags rules)
  in
  more_bags |> List.fold ~init:containing_bags ~f:Hash_set.union

let day07a rules =
  let rules = Hashtbl.of_alist_exn (module String) rules in
  let containing_bags = find_containing_bags rules "shiny gold" in
  Hash_set.length containing_bags

let rec count_bags rules bag =
  let data = Hashtbl.find_exn rules bag in
  data
  |> List.map ~f:(fun (value, bag) -> value + (value * count_bags rules bag))
  |> List.sum (module Int) ~f:Fn.id

let day07b rules =
  let rules = Hashtbl.of_alist_exn (module String) rules in
  count_bags rules "shiny gold"

let read_rules () =
  let open Stdio in
  let file = In_channel.create "input.txt" in
  In_channel.input_lines file |> List.map ~f:rule_from_string

let () =
  let rules = read_rules () in
  day07b rules |> Stdio.printf "%d\n"
