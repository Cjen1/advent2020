open! Core
open! Async
open! Lib

let get_some = function Some v -> v | None -> assert false

let run input =
  let%bind reader = Reader.open_file input in
  let%bind adapters =
    Angstrom_async.parse Angstrom.(sep_by1 end_of_line P.integer) reader
    >>| Result.ok_or_failwith
  in
  print_s [%message (adapters |> Lib.input |> Lib.paths : int)];
  Reader.close reader

let cmd =
  Command.async ~summary:""
    Command.Let_syntax.(
      let%map_open input = anon ("filename" %: Filename.arg_type) in
      fun () -> run input)

let () = Command.run cmd
