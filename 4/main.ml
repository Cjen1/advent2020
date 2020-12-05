open! Core
open! Angstrom
open! Async

module P = struct
  open Angstrom
  module S = Set.Make (String)
  open Let_syntax

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| Int.of_string

  type field = BYR | IYR | EYR | HGT | HCL | ECL | PID | CID
  [@@deriving sexp, equal]

  let value = take_till (function ' ' | '\n' -> true | _ -> false)

  let byr =
    let%bind _ = string "byr" *> char ':' in
    let%bind value = value in
    let valid =
      match parse_string ~consume:Consume.All integer value with
      | Ok i when 1920 <= i && i <= 2002 -> true
      | _ -> false
    in
    return @@ (BYR, valid)

  let () =
    let byr = parse_string ~consume:Consume.All byr "byr:1980" in
    print_s [%message (byr : (field * bool, string) Result.t)]

  let iyr =
    let%bind _ = string "iyr" *> char ':' in
    let%bind value = value in
    let valid =
      match parse_string ~consume:Consume.All integer value with
      | Ok i when 2010 <= i && i <= 2020 -> true
      | _ -> false
    in
    return (IYR, valid)

  let () =
    let iyr = parse_string ~consume:Consume.All iyr "iyr:2012" in
    print_s [%message (iyr : (field * bool, string) Result.t)]

  let eyr =
    let%bind _ = string "eyr" *> char ':' in
    let%bind value = value in
    let valid =
      match parse_string ~consume:Consume.All integer value with
      | Ok i when 2020 <= i && i <= 2030 -> true
      | _ -> false
    in
    return (EYR, valid)

  let () =
    let eyr = parse_string ~consume:Consume.All eyr "eyr:2025" in
    print_s [%message (eyr : (field * bool, string) Result.t)]

  let hgt =
    let%bind _ = string "hgt" *> char ':' in
    let%bind value = value in
    let height_cm =
      let%bind h = integer in
      let%bind cm_in =
        choice
          [ (string "cm" >>| fun _ -> `CM); (string "in" >>| fun _ -> `IN) ]
      in
      return (h, cm_in)
    in
    let valid =
      match parse_string ~consume:Consume.All height_cm value with
      | Ok (h, `CM) when 150 <= h && h <= 193 -> true
      | Ok (h, `IN) when 59 <= h && h <= 76 -> true
      | _ -> false
    in
    return (HGT, valid)

  let () =
    let hgt = parse_string ~consume:Consume.All hgt "hgt:160cm" in
    print_s [%message (hgt : (field * bool, string) Result.t)]

  let hcl =
    let%bind _ = string "hcl" *> char ':' in
    let%bind value = value in
    let col =
      let%bind (_ : char) = char '#' in
      let%bind str =
        take_while (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
      in
      if String.length str <> 6 then fail str else return str
    in
    let valid =
      match parse_string ~consume:Consume.All col value with
      | Ok _ -> true
      | _ -> false
    in
    return (HCL, valid)

  let () =
    let hcl = parse_string ~consume:Consume.All hcl "hcl:#123abc" in
    print_s [%message (hcl : (field * bool, string) Result.t)]

  let ecl =
    let%bind _ = string "ecl" *> char ':' in
    let%bind value = value in
    let col =
      choice
        [
          string "amb";
          string "blu";
          string "brn";
          string "gry";
          string "grn";
          string "hzl";
          string "oth";
        ]
    in
    let valid =
      match parse_string ~consume:Consume.All col value with
      | Ok _ -> true
      | _ -> false
    in
    return (ECL, valid)

  let () =
    let ecl = parse_string ~consume:Consume.All ecl "ecl:amb" in
    print_s [%message (ecl : (field * bool, string) Result.t)]

  let pid =
    let%bind _ = string "pid" *> char ':' in
    let%bind value = value in
    let valid =
      match parse_string ~consume:Consume.All integer value with
      | Ok _ when String.length value = 9 -> true
      | _ -> false
    in
    return (PID, valid)

  let () =
    let pid = parse_string ~consume:Consume.All pid "pid:000000001" in
    print_s [%message (pid : (field * bool, string) Result.t)]

  let cid =
    let%bind _ = string "cid" *> char ':' in
    let%bind _ = value in
    return (CID, true)

  let field = Angstrom.choice [ byr; iyr; eyr; hgt; hcl; ecl; pid; cid ]

  let fields =
    sep_by1
      (Angstrom.choice
         [ end_of_line; skip (function ' ' -> true | _ -> false) ])
      field

  let parse_passports = sep_by1 (end_of_line *> end_of_line) fields
end

open! P

let valid_passport1 p =
  let p = List.map ~f:fst p in
  let mem = List.mem p ~equal:[%equal: field] in
  mem BYR && mem IYR && mem EYR && mem HGT && mem HCL && mem ECL && mem PID

let valid_passport2 p =
  let mem = List.mem p ~equal:[%equal: field * bool] in
  mem (BYR, true)
  && mem (IYR, true)
  && mem (EYR, true)
  && mem (HGT, true)
  && mem (HCL, true)
  && mem (ECL, true)
  && mem (PID, true)

let run input =
  let%bind reader = Reader.open_file input in
  let%bind passports = Angstrom_async.parse parse_passports reader in
  let passports = passports |> Result.ok_or_failwith in
  let count = List.count passports ~f:valid_passport1 in
  print_s [%message (count : int)];
  Reader.close reader

let cmd =
  Command.async ~summary:""
    Command.Let_syntax.(
      let%map_open input = anon ("filename" %: Filename.arg_type) in
      fun () -> run input)

let () = Command.run cmd
