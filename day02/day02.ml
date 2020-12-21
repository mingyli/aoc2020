open Base
open Stdio

let pattern = Str.regexp "\\([0-9]+\\)-\\([0-9]+\\) \\([a-z]\\): \\([a-z]+\\)"

let parse line =
  let _n = Str.search_forward pattern line 0 in
  let low = Int.of_string (Str.matched_group 1 line) in
  let high = Int.of_string (Str.matched_group 2 line) in
  let policy = (Str.matched_group 3 line).[0] in
  let password = Str.matched_group 4 line in
  (low, high, policy, password)

let read_inputs () =
  let file = In_channel.create "input" in
  In_channel.input_lines file |> List.map ~f:parse

let day02a inputs =
  List.count inputs ~f:(fun (low, high, policy, password) ->
      let count = String.count password ~f:(Char.equal policy) in
      low <= count && count <= high)

let day02b inputs =
  List.count inputs ~f:(fun (low, high, policy, password) ->
      let a = password.[low - 1] in
      let b = password.[high - 1] in
      Bool.( <> ) (Char.equal a policy) (Char.equal b policy))

let () =
  let inputs = read_inputs () in
  day02b inputs |> printf "%d\n"
