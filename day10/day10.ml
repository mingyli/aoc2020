open Base

let read_numbers () =
  let open Stdio in
  let file = In_channel.create "input.txt" in
  In_channel.input_lines file |> List.map ~f:Int.of_string

let count_occurrences target numbers =
  let l1 = List.drop_last_exn numbers in
  let l2 = List.drop numbers 1 in
  List.zip_exn l1 l2
  |> List.map ~f:(fun (a, b) -> b - a)
  |> List.count ~f:(( = ) target)

let day10a numbers =
  let numbers =
    (3 + Option.value_exn (List.max_elt ~compare:Int.compare numbers))
    :: 0 :: numbers
  in
  let numbers = List.sort numbers ~compare:Int.compare in
  let ones = count_occurrences 1 numbers in
  let threes = count_occurrences 3 numbers in
  ones * threes

(**
  let rec _count_ways = function
    | [] | [ _ ] -> 1
    | [ first; second ] -> if second - first <= 3 then 1 else 0
    | first :: (second :: (third :: (fourth :: _ as tail3) as tail2) as tail1)
      ->
        let a = if second - first <= 3 then _count_ways tail1 else 0 in
        let b = if third - first <= 3 then _count_ways tail2 else 0 in
        let c = if fourth - first <= 3 then _count_ways tail3 else 0 in
        a + b + c
    | first :: (second :: (third :: _ as tail2) as tail1) ->
        let a = if second - first <= 3 then _count_ways tail1 else 0 in
        let b = if third - first <= 3 then _count_ways tail2 else 0 in
        a + b
  in
   *)
let day10b numbers =
  let count_ways numbers =
    let cache = Hashtbl.create (module Int) in
    let rec aux index =
      let result =
        match Hashtbl.find cache index with
        | Some value -> value
        | None -> (
            let n = Array.length numbers in
            match index with
            | i when i = n -> 1
            | i when i = n - 1 -> 1
            | i when i = n - 2 ->
                let a = numbers.(i) in
                let b = numbers.(i + 1) in
                if b - a <= 3 then 1 else 0
            | i when i = n - 3 ->
                let a = numbers.(i) in
                let b = numbers.(i + 1) in
                let c = numbers.(i + 2) in
                let x = if b - a <= 3 then aux (i + 1) else 0 in
                let y = if c - a <= 3 then aux (i + 2) else 0 in
                x + y
            | i ->
                let a = numbers.(i) in
                let b = numbers.(i + 1) in
                let c = numbers.(i + 2) in
                let d = numbers.(i + 3) in
                let x = if b - a <= 3 then aux (i + 1) else 0 in
                let y = if c - a <= 3 then aux (i + 2) else 0 in
                let z = if d - a <= 3 then aux (i + 3) else 0 in
                x + y + z )
      in
      let (_ : unit) = Hashtbl.set cache ~key:index ~data:result in
      result
    in
    aux 0
  in
  let numbers =
    (3 + Option.value_exn (List.max_elt ~compare:Int.compare numbers))
    :: 0 :: numbers
  in
  let numbers = List.sort numbers ~compare:Int.compare in
  count_ways (List.to_array numbers)

let () =
  let numbers = read_numbers () in
  day10b numbers |> Stdio.printf "%d\n"
