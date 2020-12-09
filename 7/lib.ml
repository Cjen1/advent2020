open! Core

module P = struct
  open Angstrom
  module S = Set.Make (String)
  open Let_syntax

  let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

  let word = take_while1 is_letter

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| Int.of_string

  type bag_or_word = Word of string | Bag

  let bag = string "bags" <|> string "bag"

  let word_not_bag =
    word >>= function "bag" | "bags" -> fail "Is a bag" | s -> return s

  let whitespace = many1 @@ (char ' ' <|> char '.')

  type description = string [@@deriving sexp]

  let description =
    let%bind des = sep_by1 whitespace word_not_bag <* whitespace <* bag in
    return @@ String.concat des ~sep:" "

  type contains = { count : int; description : description } [@@deriving sexp]

  let rule_contains =
    let%bind count = integer <* whitespace in
    let%bind description = description in
    return { count; description }

  type rule = { container : description; contains : contains list }
  [@@deriving sexp]

  let rule =
    let%bind container = description in
    let%bind _contain = whitespace <* string "contain" <* whitespace in
    let%bind contains =
      sep_by1 (string ", ") rule_contains
      <|> string "no other bags" *> return []
    in
    let%bind _eol = whitespace in
    return { container; contains }
end

open! P

let%expect_test "parser" =
  let parse p s =
    let open Angstrom in
    parse_string ~consume:Consume.All p s |> Result.ok_or_failwith
  in
  let v = parse P.description "abc d bags" in
  print_s [%message (v : P.description)];
  [%expect {| (v "abc d") |}];
  let v = parse P.description "blue bag" in
  print_s [%message (v : description)];
  [%expect {| (v blue) |}];
  let v =
    parse P.rule
      "light red bags contain 1 bright white bag, 2 muted yellow bags."
  in
  print_s [%message (v : P.rule)];
  [%expect
    {|
    (v
     ((container "light red")
      (contains
       (((count 1) (description "bright white"))
        ((count 2) (description "muted yellow")))))) |}];
  let v = parse P.rule "bright white bags contain 1 shiny gold bag." in
  print_s [%message (v : P.rule)];
  [%expect
    {|
    (v
     ((container "bright white")
      (contains (((count 1) (description "shiny gold")))))) |}]

let split ls p =
  let t, f =
    List.fold_left ls ~init:([], []) ~f:(fun (t, f) v ->
        match p v with Ok v -> (v :: t, f) | Error v -> (t, v :: f))
  in
  (List.rev t, List.rev f)

let%expect_test "split" =
  let v =
    split [ 1; 2; 3; 4; 5; 6; 7; 8 ] (function
      | v when v % 3 = 0 -> Ok v
      | v -> Error v)
  in
  print_s [%message (v : int list * int list)];
  [%expect {| (v ((3 6) (1 2 4 5 7 8))) |}]

let rec reachable reached remaining =
  let p rule =
    let can_contain =
      List.find rule.P.contains ~f:(fun { description; _ } ->
          List.mem reached description ~equal:String.equal)
    in
    match can_contain with Some _ -> Ok rule.P.container | None -> Error rule
  in
  match split remaining p with
  | [], _ -> reached
  | l, remaining -> reachable (l @ reached) remaining

let%expect_test "reachable" =
  let v =
    "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."
  in
  let rules =
    Angstrom.(parse_string ~consume:Consume.All (sep_by1 end_of_line P.rule) v)
    |> Result.ok_or_failwith
  in
  let v = reachable ["shiny gold"] rules in
  print_s [%message (v: string list)];
  [%expect {| (v ("light red" "dark orange" "bright white" "muted yellow" "shiny gold")) |}]

module MS = Map.Make(String)
let count_within init rules =
  let rec loop above_count non_emptied = 
    let new_empties = Map.fold non_emptied ~init:MS.empty ~f:(fun ~key:description ~data:multiple m ->
        let contains = MS.find_exn rules description in
        List.fold contains ~init:m ~f:(fun m {count; description} ->
            let count = multiple * count in
            MS.update m description ~f:(function
                | Some prev -> count + prev
                | None -> count
              )
          )
      )
    in 
    let non_emptied_count = 
      MS.fold non_emptied ~init:0 ~f:(fun ~key:_ ~data v -> v + data)
    in 
    match () with
    | () when MS.is_empty new_empties -> non_emptied_count + above_count
    | _ -> loop (above_count + non_emptied_count) new_empties
  in 
  let init = MS.empty |> MS.set ~key:init ~data:1 in
  loop 0 init

let%expect_test "count_within" =
  let v =
    "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags."
  in
  let rules =
    Angstrom.(parse_string ~consume:Consume.All (sep_by1 end_of_line P.rule) v)
    |> Result.ok_or_failwith
  in
  let rules = List.map rules ~f:(fun {container; contains} -> container, contains) in
  let rules = MS.of_alist_exn rules in
  let v = count_within "shiny gold" rules in
  print_s [%message (v: int)];
  [%expect{| (v 127) |}]

