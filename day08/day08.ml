open Base

type instruction = Acc of int | Jmp of int | Nop of int

let instruction_of_string s =
  let pattern =
    Re.seq
      [
        Re.group (Re.alt [ Re.str "acc"; Re.str "jmp"; Re.str "nop" ]);
        Re.str " ";
        Re.group (Re.seq [ Re.set "+-"; Re.rep Re.digit ]);
      ]
    |> Re.compile
  in
  let groups = Re.exec pattern s in
  let argument = Re.Group.get groups 2 |> Int.of_string in
  match Re.Group.get groups 1 with
  | "acc" -> Acc argument
  | "jmp" -> Jmp argument
  | "nop" -> Nop argument
  | _ -> assert false

let read_instructions () =
  let open Stdio in
  let file = In_channel.create "input.txt" in
  In_channel.input_lines file
  |> List.map ~f:instruction_of_string
  |> Array.of_list

let day08a instructions =
  let rec simulate pos visited accumulator =
    if Set.mem visited pos then accumulator
    else
      let visited = Set.add visited pos in
      let instruction = instructions.(pos) in
      match instruction with
      | Acc delta -> simulate (pos + 1) visited (accumulator + delta)
      | Jmp offset -> simulate (pos + offset) visited accumulator
      | Nop _ -> simulate (pos + 1) visited accumulator
  in
  simulate 0 (Set.empty (module Int)) 0

let day08b instructions =
  let rec simulate pos visited accumulator =
    if pos = Array.length instructions then Some accumulator
    else if Set.mem visited pos then None
    else
      let visited = Set.add visited pos in
      match instructions.(pos) with
      | Acc delta -> simulate (pos + 1) visited (accumulator + delta)
      | Jmp offset -> simulate (pos + offset) visited accumulator
      | Nop _ -> simulate (pos + 1) visited accumulator
  in
  let range = List.init (Array.length instructions) ~f:(fun i -> i) in
  range
  |> List.map ~f:(fun index ->
         let old_instruction = instructions.(index) in
         let new_instruction =
           match old_instruction with
           | Jmp offset -> Nop offset
           | Nop offset -> Jmp offset
           | Acc delta -> Acc delta
         in
         let () = instructions.(index) <- new_instruction in
         let result = simulate 0 (Set.empty (module Int)) 0 in
         let () = instructions.(index) <- old_instruction in
         result)
  |> List.find_map ~f:(fun o -> o)
  |> Option.value_exn

let () =
  let instructions = read_instructions () in
  day08b instructions |> Stdio.printf "%d\n"
