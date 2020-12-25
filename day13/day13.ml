open Base

let read_schedule () =
  let open Stdio in
  let file = In_channel.create "input.txt" in
  let arrival_ts = In_channel.input_line_exn file |> Int.of_string in
  let buses =
    In_channel.input_line_exn file
    |> String.split_on_chars ~on:[ ',' ]
    |> List.map ~f:(function "x" -> None | id -> Some (Int.of_string id))
  in
  (arrival_ts, buses)

let day13a (arrival_ts, buses) =
  let find_compatible_bus ts =
    List.find_map buses ~f:(function
      | Some id when ts % id = 0 -> Some (ts, id)
      | _ -> None)
  in
  let earliest_ts, bus_id =
    List.init 1000 ~f:(fun i -> i + arrival_ts)
    |> List.find_map_exn ~f:find_compatible_bus
  in
  let wait = earliest_ts - arrival_ts in
  wait * bus_id

(*
Return ( x, y )
such that GCD(a, b) = ax + by
*)
let rec egcd a b =
  if b = 0 then (1, 0)
  else
    let quotient = a / b in
    let remainder = a % b in
    let s, t = egcd b remainder in
    (t, s - (quotient * t))

(*
Return x such that
num * x = 1 (mod modulus)
*)
let multiplicative_inverse modulus num =
  let x, y = egcd num modulus in
  if (num * x) + (modulus * y) = 1 then Some x else None

let chinese_remainder_theorem moduli remainders =
  let product = List.reduce_exn moduli ~f:( * ) in
  let partial_products =
    List.map moduli ~f:(fun modulus -> product / modulus)
  in
  let inverses =
    List.map2_exn partial_products moduli ~f:(fun partial_product modulus ->
        multiplicative_inverse modulus partial_product |> Option.value_exn)
  in
  List.map3_exn remainders partial_products inverses
    ~f:(fun remainder partial_product inverse ->
      remainder * partial_product * inverse)
  |> List.sum (module Int) ~f:Fn.id
  |> fun sum -> sum % product

let day13b (_, buses) =
  let buses =
    List.filter_mapi buses ~f:(fun offset -> function
      | None -> None | Some id -> Some (id - offset, id))
  in
  let remainders, bus_ids = List.unzip buses in
  chinese_remainder_theorem bus_ids remainders

let () =
  let arrival_ts, buses = read_schedule () in
  day13b (arrival_ts, buses) |> Stdio.printf "%d\n"
