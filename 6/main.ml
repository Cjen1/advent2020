open! Core
open! Async
open! Lib

let get_some = function Some v -> v | None -> assert false

let rec fold_left_2 ~f ~init l =
  match l with
  | a :: (b :: _ as rem) -> fold_left_2 ~f ~init:(f init a b) rem
  | remainder -> (init, remainder)

module S = Set.Make(Char)

let run input =
  let%bind reader = Reader.open_file input in
  let%bind data =
    Angstrom_async.parse
      Angstrom.(sep_by1 (end_of_line *> end_of_line) P.data)
      reader
    >>| Result.ok_or_failwith
  in
  let res =
    List.map data ~f:(fun group ->
        let sets = List.map group ~f:(fun person -> S.of_list person) in
        let set_inter =  List.fold (List.tl_exn sets) ~init:(List.hd_exn sets) ~f:Set.inter in
        S.length set_inter
      )
  in 
  let res = List.fold res ~init:0 ~f:(fun c i -> c + i) in
  print_s [%message (res : int)];
  Reader.close reader

let cmd =
  Command.async ~summary:""
    Command.Let_syntax.(
      let%map_open input = anon ("filename" %: Filename.arg_type) in
      fun () -> run input)

let () = Command.run cmd
