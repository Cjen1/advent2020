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

let small_adapters = [ 16; 10; 15; 5; 1; 11; 7; 19; 6; 12; 4 ]

let large_adapters =
  [
    28;
    33;
    18;
    42;
    31;
    14;
    46;
    20;
    48;
    47;
    24;
    23;
    49;
    45;
    19;
    38;
    39;
    11;
    1;
    32;
    25;
    35;
    8;
    17;
    7;
    9;
    4;
    2;
    34;
    10;
    3;
  ]

let input adapters =
  let built_in = List.fold ~init:0 ~f:Int.max adapters |> fun v -> v + 3 in
  List.sort ~compare:Int.compare (built_in :: adapters)

let%expect_test "input" =
  print_s [%message (input [ 1; 2; 3 ] : int list)];
  [%expect {| ("input [1; 2; 3]" (1 2 3 6)) |}]

type diff = { ones : int; threes : int } [@@deriving sexp]

let differences adapters =
  List.fold_left adapters
    ~init:({ ones = 0; threes = 0 }, 0)
    ~f:(fun ({ ones; threes }, prev) curr ->
      let acc =
        match curr - prev with
        | 1 -> { ones = ones + 1; threes }
        | 2 -> { ones; threes }
        | 3 -> { ones; threes = threes + 1 }
        | _ ->
            [%message (curr : int) (prev : int) (curr - prev : int)]
            |> Sexp.to_string_hum |> failwith
      in
      (acc, curr))
  |> fst

let%expect_test "p1" =
  print_s [%message (small_adapters |> input |> differences : diff)];
  [%expect {| ("(small_adapters |> input) |> differences" ((ones 7) (threes 5))) |}];
  print_s [%message (large_adapters |> input |> differences : diff)];
  [%expect {| ("(large_adapters |> input) |> differences" ((ones 22) (threes 10))) |}]

type prev = {rating : int ; options : int} [@@deriving sexp]
let paths adapters =
  let get_options curr_rating = function
    | [] -> 1
    | a :: b :: c :: _ when curr_rating - c.rating <= 3 -> a.options + b.options + c.options
    | a :: b :: _ when curr_rating - b.rating <= 3 -> a.options + b.options
    | a :: _ when curr_rating - a.rating <= 3 -> a.options
    | l -> [%message (l : prev list)] |> Sexp.to_string_hum |> failwith
  in 
  let rec loop adapters prev_options =
    match adapters with
    | [] -> (prev_options |> List.hd_exn).options
    | x :: xs -> 
      let options = get_options x prev_options in
      loop xs ({options; rating=x} :: prev_options)
  in loop adapters [{rating=0; options=1}]

let%expect_test "p1" =
  print_s [%message (small_adapters |> input |> paths : int)];
  [%expect {|
    ("(small_adapters |> input) |> paths" 8) |}];
  print_s [%message (large_adapters |> input |> paths : int)];
  [%expect {|
    ("(large_adapters |> input) |> paths" 19208) |}]

