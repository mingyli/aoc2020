open Base

type instruction = SetMask of string | SetMem of int * int

let instruction_of_string s =
  let pattern =
    Re.alt
      [
        Re.seq [ Re.str "mask = "; Re.group (Re.rep (Re.set "01X")) ];
        Re.seq
          [
            Re.seq [ Re.str "mem["; Re.group (Re.rep Re.digit); Re.str "]" ];
            Re.str " = ";
            Re.group (Re.rep Re.digit);
          ];
      ]
    |> Re.compile
  in
  let groups = Re.exec pattern s in
  if Re.Group.test groups 1 then
    let mask = Re.Group.get groups 1 in
    SetMask mask
  else
    let address = Re.Group.get groups 2 |> Int.of_string in
    let value = Re.Group.get groups 3 |> Int.of_string in
    SetMem (address, value)

let apply_mask mask value =
  List.init 36 ~f:Fn.id
  |> List.fold ~init:0 ~f:(fun result index ->
         let bit = 1 lsl (35 - index) in
         match mask.[index] with
         | 'X' -> result + (value land bit)
         | '0' -> result
         | '1' -> result + bit
         | _ -> assert false)

let day14a instructions =
  let _mask, memory =
    instructions
    |> List.fold
         ~init:("", Hashtbl.create (module Int))
         ~f:(fun (mask, memory) instruction ->
           match instruction with
           | SetMask mask -> (mask, memory)
           | SetMem (address, value) ->
               let new_value = apply_mask mask value in
               Hashtbl.set memory ~key:address ~data:new_value;
               (mask, memory))
  in
  Hashtbl.data memory |> List.sum (module Int) ~f:Fn.id

let apply_mask mask address =
  let rec aux index address =
    if index = 36 then [ address ]
    else
      let bit = 1 lsl (35 - index) in
      let address' = address lor bit in
      let address'' = address land lnot bit in
      match mask.[index] with
      | '0' -> aux (index + 1) address
      | '1' -> aux (index + 1) address'
      | 'X' ->
          List.concat [ aux (index + 1) address'; aux (index + 1) address'' ]
      | _ -> assert false
  in
  aux 0 address

let day14b instructions =
  let _mask, memory =
    instructions
    |> List.fold
         ~init:("", Hashtbl.create (module Int))
         ~f:(fun (mask, memory) instruction ->
           match instruction with
           | SetMask mask -> (mask, memory)
           | SetMem (address, value) ->
               let addresses = apply_mask mask address in
               List.iter addresses ~f:(fun address ->
                   Hashtbl.set memory ~key:address ~data:value);
               (mask, memory))
  in
  Hashtbl.data memory |> List.sum (module Int) ~f:Fn.id

let read_instructions () =
  let open Stdio in
  let file = In_channel.create "input.txt" in
  In_channel.input_lines file |> List.map ~f:instruction_of_string

let () =
  let instructions = read_instructions () in
  day14b instructions |> Stdio.printf "%d\n"
