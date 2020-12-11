open! Core
open! Async
open! Lib

let get_some = function Some v -> v | None -> assert false

let run input =
  let%bind reader = Reader.open_file input in
  let%bind seats =
    Angstrom_async.parse P.input reader >>| Result.ok_or_failwith
  in
  let final_seats = fix advance' seats (Map.equal P.equal_state) in
  let count = Map.fold final_seats ~init:0 ~f:(fun ~key:_ ~data c ->
      match data with
      | P.Occupied -> c + 1
      | P.Empty -> c
    ) in
  print_s [%message (count : int)];
  Reader.close reader

let cmd =
  Command.async ~summary:""
    Command.Let_syntax.(
      let%map_open input = anon ("filename" %: Filename.arg_type) in
      fun () -> run input)

let () = Command.run cmd
