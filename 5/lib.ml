open! Core

module P = struct
  open Angstrom
  module S = Set.Make (String)
  open Let_syntax

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| Int.of_string

  type fb = F | B
  type lr = L | R

  let fb = peek_char >>= function
    | Some 'F' -> advance 1 *> return F
    | Some 'B' -> advance 1 *> return B
    | _ -> fail "Incorrect char"

  let lr = peek_char >>= function
    | Some 'L' -> advance 1 *> return L
    | Some 'R' -> advance 1 *> return R
    | _ -> fail "Incorrect char"

  let data = 
    let%bind row = count 7 fb in
    let%bind col = count 3 lr in
    return (row, col)
end

open! P

let parse_bin_int s parse_char =
  List.fold_left s ~init:0 ~f:(fun i v ->
      let v = parse_char v in
      (i lsl 1) lor v
    )

let%expect_test "parse_bin_int" =
  let res = parse_bin_int [1;0;0] ident in
  print_s [%message (res : int)];
  [%expect {| (res 4) |}];
  let res = parse_bin_int [0;1;0;1] ident in
  print_s [%message (res : int)];
  [%expect {| (res 5) |}]

let parse_fb = function F -> 0 | B -> 1
let parse_lr = function L -> 0 | R -> 1

let parse_row_col (row, col) = 
  let row = parse_bin_int row (parse_fb) in
  let col = parse_bin_int col (parse_lr) in
  row * 8 + col

let%expect_test "parse_row_col" =
  let v = [B;F;F;F;B;B;F],[R;R;R] in
  print_s [%message (parse_bin_int (fst v) (parse_fb) : int)];
  print_s [%message (parse_bin_int (snd v) (parse_lr) : int)];
  print_s [%message (parse_row_col v : int)];
  [%expect {||}];
  let v = [F;F;F;B;B;B;F],[R;R;R] in
  print_s [%message (parse_bin_int (fst v) (parse_fb) : int)];
  print_s [%message (parse_bin_int (snd v) (parse_lr) : int)];
  print_s [%message (parse_row_col v : int)];
  [%expect {||}];
  let v = [B;B;F;F;B;B;F],[R;L;L] in
  print_s [%message (parse_bin_int (fst v) (parse_fb) : int)];
  print_s [%message (parse_bin_int (snd v) (parse_lr) : int)];
  print_s [%message (parse_row_col v : int)];
  [%expect {||}];
