open Base

let read_numbers () =
  let open Stdio in
  let file = In_channel.create "input.txt" in
  In_channel.input_lines file |> List.map ~f:Int.of_string

let day09a numbers =
  let any_pair_sum nums target =
    let cartesian l l' =
      List.concat (List.map ~f:(fun e -> List.map ~f:(fun e' -> (e, e')) l') l)
    in
    let pairs = cartesian nums nums in
    List.exists pairs ~f:(fun (a, b) -> a + b = target)
  in
  numbers
  |> List.fold_until
       ~init:(Queue.create ~capacity:25 ())
       ~f:(fun queue num ->
         if Queue.length queue < 25 then
           let (_ : unit) = Queue.enqueue queue num in
           Continue queue
         else if any_pair_sum (Queue.to_list queue) num then
           let (_ : int) = Queue.dequeue_exn queue in
           let (_ : unit) = Queue.enqueue queue num in
           Continue queue
         else Stop num)
       ~finish:(fun _ -> assert false)

let day09b numbers =
  let target = 105950735 in
  let queue = Queue.create () in
  numbers
  |> List.fold_until ~init:0
       ~f:(fun sum num ->
         let rec pop_queue_excess sum =
           if sum > target then
             let popped = Queue.dequeue_exn queue in
             pop_queue_excess (sum - popped)
           else sum
         in
         let sum = pop_queue_excess sum in
         if sum = target then Stop queue
         else
           let (_ : unit) = Queue.enqueue queue num in
           Continue (sum + num))
       ~finish:(fun _ -> assert false)
  |> fun queue ->
  let min = Queue.min_elt queue ~compare:Int.compare |> Option.value_exn in
  let max = Queue.max_elt queue ~compare:Int.compare |> Option.value_exn in
  min + max

let () =
  let numbers = read_numbers () in
  day09b numbers |> Stdio.printf "%d\n"
