open! Core

module P = struct
  open Angstrom
  module S = Set.Make (String)
  open Let_syntax

  let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

  let word = take_while1 is_letter

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| Int.of_string

  let signed_integer =
    peek_char_fail >>= function
    | '+' -> advance 1 *> integer
    | '-' -> advance 1 *> integer >>| fun i -> - i
    | _ -> fail "Not recognised sign"

  type instruction = Nop of int | Jmp of int | Acc of int [@@deriving sexp]

  let nop = string "nop" *> char ' ' *> signed_integer >>| fun i -> Nop i
  let jmp = string "jmp" *> char ' ' *> signed_integer >>| fun i -> Jmp i
  let acc = string "acc" *> char ' ' *> signed_integer >>| fun i -> Acc i

  let instruction = nop <|> jmp <|> acc

  let instructions = sep_by1 end_of_line instruction
end

open! P

let%expect_test "instruction" =
  let s = "nop +0" in
  let v = Angstrom.(parse_string ~consume:Consume.All P.instruction s) |> Result.ok_or_failwith in
  print_s [%message (v : instruction)];
  [%expect {| (v Nop) |}];
  let s = "acc +1" in
  let v = Angstrom.(parse_string ~consume:Consume.All P.instruction s) |> Result.ok_or_failwith in
  print_s [%message (v : instruction)];
  [%expect {| (v (Acc 1)) |}];
  let s = "jmp -5" in
  let v = Angstrom.(parse_string ~consume:Consume.All P.instruction s) |> Result.ok_or_failwith in
  print_s [%message (v : instruction)];
  [%expect {| (v (Jmp -5)) |}]

let single_step ins i acc =
  match ins.(i) with
  | Nop _ -> (i + 1, acc)
  | Jmp j -> (i + j, acc)
  | Acc j -> (i + 1, acc + j)

module IS = Hash_set.Make(Int)

let execute ins =
  let visited = IS.create () in
  let rec loop i acc =
    match Hash_set.mem visited i with
    | _ when i >= Array.length ins -> Ok acc
    | true -> Error acc
    | false ->
      let i', acc' = single_step ins i acc in
      Hash_set.add visited i;
      loop i' acc'
  in loop 0 0

let%expect_test "execute" =
  let s = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6" in
  let v = Angstrom.(parse_string ~consume:Consume.All P.instructions s) |> Result.ok_or_failwith in
  print_s [%message (v : instruction list)];
  [%expect {| (v (Nop (Acc 1) (Jmp 4) (Acc 3) (Jmp -3) (Acc -99) (Acc 1) (Jmp -4) (Acc 6))) |}];
  let ins = Array.of_list v in
  print_s [%message (execute ins : (int, int) Result.t)];
  [%expect {| ("execute_til_loop ins" 5) |}]

let find_single_edit ins =
  let perform_edit i v' =
    let ins' = Array.copy ins in
    Array.set ins' i v';
    match execute ins' with
    | Ok acc -> Some (i,acc)
    | Error _ -> None
  in 
  Array.find_mapi_exn ins ~f:(fun i v ->
      match v with
      | Acc _ -> None
      | Nop j when i + j >= Array.length ins -> None
      | Nop j -> perform_edit i (Jmp j)
      | Jmp j -> perform_edit i (Nop j)
    )

let%expect_test "find_edit" =
  let s = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6" in
  let v = Angstrom.(parse_string ~consume:Consume.All P.instructions s) |> Result.ok_or_failwith in
  let ins = Array.of_list v in
  let edit_index, acc = find_single_edit ins in 
  print_s [%message (edit_index : int) (acc : int)];
  [%expect {||}]
