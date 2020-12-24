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

let memoize m f =
  let cache = Hashtbl.create m in
  fun x -> Hashtbl.find_or_add cache x ~default:(fun () -> f x)

let memoize_rec m f_norec x =
  let fref = ref (fun _ -> assert false) in
  let f = memoize m (fun x -> f_norec !fref x) in
  fref := f;
  f x

let day10b numbers =
  let count_ways numbers =
    let n = Array.length numbers in
    let aux =
      memoize_rec
        (module Int)
        (fun aux index ->
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
              x + y + z)
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
