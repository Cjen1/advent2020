open! Core

module P = struct
  open Angstrom
  module S = Set.Make (String)
  open Let_syntax

  let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

  let word = take_while1 is_letter

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| Int.of_string
end

open! P
module S = Set.Make (Int)

let check_valid arr preamble_size i =
  let v = arr.(i) in
  let rec loop_check s pi =
    if pi >= i then None
    else
      let vpi = arr.(pi) in
      match S.mem s (v - vpi) with
      | true -> Some (vpi, v - vpi)
      | false -> loop_check (S.add s vpi) (pi + 1)
  in
  match loop_check S.empty (i - preamble_size) with
  | Some _ -> true
  | None -> false

let%expect_test "check_valid" =
  let arr = [| 1; 2; 1; 4; 5 |] in
  print_s [%message (check_valid arr 4 4 : bool)];
  [%expect {|("check_valid arr 4 4" true) |}]

let find_incorrect arr preamble_size =
  Array.findi arr ~f:(fun i _ ->
      i > preamble_size && not @@ check_valid arr preamble_size i)

let%expect_test "find_incorrect" =
  let arr =
    [|
      35;
      20;
      15;
      25;
      47;
      40;
      62;
      55;
      65;
      95;
      102;
      117;
      150;
      182;
      127;
      219;
      299;
      277;
      309;
      576;
    |]
  in
  print_s [%message (find_incorrect arr 5 : (int * int) option)];
  [%expect {| ("find_incorrect arr 5" ((14 127))) |}]
