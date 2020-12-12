open! Core
open! Async
open! Lib

let get_some = function Some v -> v | None -> assert false

let run input =
  let%bind reader = Reader.open_file input in
  let%bind input =
    Angstrom_async.parse P.input reader >>| Result.ok_or_failwith
  in
  let b = List.fold input ~init:Two.start ~f:Two.apply in
  print_s [%message (b : Two.boat) (abs b.loc.n + abs b.loc.e : int)];
  Reader.close reader

let cmd =
  Command.async ~summary:""
    Command.Let_syntax.(
      let%map_open input = anon ("filename" %: Filename.arg_type) in
      fun () -> run input)

let () = Command.run cmd
