open Base
open Stdio

let read_passports () =
  let file = In_channel.create "input.txt" in
  In_channel.input_lines file
  |> List.group ~break:(fun _ line -> String.equal line "")
  |> List.map ~f:(List.filter ~f:(fun s -> not (String.is_empty s)))
  |> List.map ~f:(List.map ~f:(String.split ~on:' '))
  |> List.map ~f:List.concat
  |> List.map ~f:(List.map ~f:(String.lsplit2_exn ~on:':'))

let day04a passports =
  let valid passport =
    let required_keys = [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ] in
    List.for_all required_keys ~f:(List.Assoc.mem ~equal:String.equal passport)
  in
  List.count passports ~f:valid

let day04b passports =
  let rules =
    [
      ( "byr",
        fun byr ->
          let byr = Int.of_string byr in
          1920 <= byr && byr <= 2002 );
      ( "iyr",
        fun iyr ->
          let iyr = Int.of_string iyr in
          2010 <= iyr && iyr <= 2020 );
      ( "eyr",
        fun eyr ->
          let eyr = Int.of_string eyr in
          2020 <= eyr && eyr <= 2030 );
      ( "hgt",
        fun hgt ->
          let units = String.sub hgt ~pos:(String.length hgt - 2) ~len:2 in
          match units with
          | "in" ->
              let height =
                String.sub hgt ~pos:0 ~len:(String.length hgt - 2)
                |> Int.of_string
              in
              59 <= height && height <= 76
          | "cm" ->
              let height =
                String.sub hgt ~pos:0 ~len:(String.length hgt - 2)
                |> Int.of_string
              in
              150 <= height && height <= 193
          | _ -> false );
      ( "hcl",
        fun hcl ->
          let pattern =
            Re.seq
              [
                Re.bos;
                Re.char '#';
                Re.repn (Re.alt [ Re.rg '0' '9'; Re.rg 'a' 'f' ]) 6 (Some 6);
                Re.eos;
              ]
            |> Re.compile
          in
          Re.exec_opt pattern hcl |> Option.is_some );
      ( "ecl",
        fun ecl ->
          let colors = [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ] in
          List.exists colors ~f:(String.equal ecl) );
      ( "pid",
        fun pid ->
          let pattern =
            Re.seq [ Re.bos; Re.repn (Re.rg '0' '9') 9 (Some 9); Re.eos ]
            |> Re.compile
          in
          Re.exec_opt pattern pid |> Option.is_some );
    ]
  in
  let valid passport =
    List.for_all rules ~f:(fun (key, rule) ->
        let value = List.Assoc.find ~equal:String.equal passport key in
        match value with None -> false | Some value -> rule value)
  in
  List.count passports ~f:valid

let () =
  let passports = read_passports () in
  day04b passports |> printf "%d\n"
