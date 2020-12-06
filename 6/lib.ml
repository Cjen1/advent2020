open! Core

module P1 = struct
  open Angstrom
  module S = Set.Make (String)
  open Let_syntax

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| Int.of_string

  let lower_case = take_while1 (function 'a' .. 'z' -> true | _ -> false) >>| String.to_list

  let data = sep_by1 end_of_line lower_case >>| List.concat
end

module P = struct
  open Angstrom
  module S = Set.Make (String)
  open Let_syntax

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| Int.of_string

  let lower_case = take_while1 (function 'a' .. 'z' -> true | _ -> false) >>| String.to_list

  let data = sep_by1 end_of_line lower_case
end
open! P

let%expect_test "parser" =
  let parse s =
    let open Angstrom in
    parse_string ~consume:Consume.All P.data s
  |> Result.ok_or_failwith
  in 
  let v = parse "abc\nd" in
  print_s [%message (v : char list list)];
  [%expect {| (v (a b c d)) |}];
  let v = parse "a\nb\na" in
  print_s [%message (v : char list list)];
  [%expect {| (v (a b a)) |}];

