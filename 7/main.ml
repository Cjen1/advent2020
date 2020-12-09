open! Core
open! Async
open! Lib

let get_some = function Some v -> v | None -> assert false

let rec fold_left_2 ~f ~init l =
  match l with
  | a :: (b :: _ as rem) -> fold_left_2 ~f ~init:(f init a b) rem
  | remainder -> (init, remainder)

module S = Set.Make (Char)

let run input =
  let%bind reader = Reader.open_file input in
  let%bind rules =
    Angstrom_async.parse Angstrom.(sep_by1 end_of_line P.rule) reader
    >>| Result.ok_or_failwith
  in
  let key_color = "shiny gold" in
  let rules = List.map rules ~f:(fun {container; contains} -> container, contains) in
  let rules = MS.of_alist_exn rules in
  let res = count_within key_color rules in
  (* Remove outer container *)
  let res = res - 1 in
  print_s [%message (res : int)];
  Reader.close reader

let cmd =
  Command.async ~summary:""
    Command.Let_syntax.(
      let%map_open input = anon ("filename" %: Filename.arg_type) in
      fun () -> run input)

let () = Command.run cmd
