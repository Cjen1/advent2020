open! Core
open! Async
open! Lib

let get_some = function Some v -> v | None -> assert false

let run input =
  let%bind reader = Reader.open_file input in
  let%bind ins =
    Angstrom_async.parse P.instructions reader
    >>| Result.ok_or_failwith
  in
  let ins = Array.of_list ins in
  let _, acc = find_single_edit ins in
  print_s [%message (acc : int)];
  Reader.close reader

let cmd =
  Command.async ~summary:""
    Command.Let_syntax.(
      let%map_open input = anon ("filename" %: Filename.arg_type) in
      fun () -> run input)

let () = Command.run cmd
