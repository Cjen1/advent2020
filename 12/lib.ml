open! Core
module Accessor = Accessor_core
open! Accessor.O

module P = struct
  open Angstrom
  open Let_syntax

  let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

  let word = take_while1 is_letter

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| Int.of_string

  type loc = { n : int; e : int } [@@deriving accessors, sexp]

  type dir = DN | DS | DE | DW [@@deriving accessors, sexp]

  type ins =
    | N of int
    | S of int
    | E of int
    | W of int
    | L of int
    | R of int
    | F of int

  let ins =
    let%bind c = any_char in
    let%bind i = integer in
    match c with
    | 'N' -> return (N i)
    | 'S' -> return (S i)
    | 'E' -> return (E i)
    | 'W' -> return (W i)
    | 'L' -> return (L i)
    | 'R' -> return (R i)
    | 'F' -> return (F i)
    | _ -> fail "Not recognised command"

  let input = sep_by1 end_of_line ins
end

open! P

let modulo x y =
  let res = x mod y in
  if res >= 0 then res else res + y

let rec applyn x f = function 0 -> x | n -> applyn (f x) f (n - 1)

module One = struct
  type boat = { dir : dir; loc : loc } [@@deriving accessors, sexp]

  let start = { dir = DE; loc = { n = 0; e = 0 } }

  let turn_left b =
    match b.dir with
    | DE -> { b with dir = DN }
    | DS -> { b with dir = DE }
    | DW -> { b with dir = DS }
    | DN -> { b with dir = DW }

  let traverse b dir i =
    match dir with
    | DN -> Accessor.map (loc @> n) b ~f:(fun v -> v + i)
    | DS -> Accessor.map (loc @> n) b ~f:(fun v -> v - i)
    | DE -> Accessor.map (loc @> e) b ~f:(fun v -> v + i)
    | DW -> Accessor.map (loc @> e) b ~f:(fun v -> v - i)

  let rec apply b ins =
    match ins with
    | N i -> traverse b DN i
    | S i -> traverse b DS i
    | E i -> traverse b DE i
    | W i -> traverse b DW i
    | F i -> traverse b b.dir i
    | L i ->
        let amount = modulo (i / 90) 4 in
        applyn b turn_left amount
    | R i -> apply b (L (-i))

  let%expect_test "apply" =
    let s = "F10\nN3\nF7\nR90\nF11" in
    let v =
      Angstrom.(parse_string ~consume:Consume.All P.input s)
      |> Result.ok_or_failwith
    in
    let b = List.fold v ~init:start ~f:apply in
    print_s [%message (b : boat)];
    [%expect {| (b ((dir DS) (loc ((n -8) (e 17))))) |}]
end

module Two = struct
  type boat = { dir : dir; loc : loc ; wp : loc } [@@deriving accessors, sexp]

  let start = { dir = DE; loc = { n = 0; e = 0 }; wp = {n = 1; e = 10} }

  let forward b i =
    let dn = b.wp.n * i in
    let de = b.wp.e * i in
    Accessor.map loc b ~f:(fun {n;e} -> {n=n+dn; e=e+de})

  let left b =
    Accessor.map wp b ~f:(fun {n;e} -> {n=e;e=(-n)})

  let rec apply b ins =
    match ins with
    | N i -> Accessor.map (wp @> n) b ~f:(fun v -> v + i)
    | S i -> Accessor.map (wp @> n) b ~f:(fun v -> v - i)
    | E i -> Accessor.map (wp @> e) b ~f:(fun v -> v + i)
    | W i -> Accessor.map (wp @> e) b ~f:(fun v -> v - i)
    | F i -> forward b i
    | L i ->
        let amount = modulo (i / 90) 4 in
        applyn b left amount
    | R i -> apply b (L (-i))

  let%expect_test "apply" =
    let s = "F10\nN3\nF7\nR90\nF11" in
    let v =
      Angstrom.(parse_string ~consume:Consume.All P.input s)
      |> Result.ok_or_failwith
    in
    let b = List.fold v ~init:start ~f:apply in
    print_s [%message (b : boat)];
    [%expect {| (b ((dir DE) (loc ((n -72) (e 214))) (wp ((n -10) (e 4))))) |}]
end
