open! Core
open! Angstrom
open! Async

module P = struct
  open Angstrom.Let_syntax

  let is_whitespace = function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false

  let whitespace = take_while is_whitespace

  let is_digit = function '0'..'9' -> true | _ -> false

  let integer =
        take_while1 is_digit >>| Int.of_string

  let input = 
    let%bind i = integer in
    return i
end

open! P

let run input = 
  let%bind reader = Reader.open_file input in
  let%bind input = Angstrom_async.parse (many1 @@ whitespace *> P.input) reader >>| Result.ok_or_failwith in
  print_s [%message (input : int list)];
  Reader.close reader

let cmd =
  Command.async 
    ~summary:""
    Command.Let_syntax.(
      let%map_open input = anon ("filename" %: Filename.arg_type) in
      fun () -> run input
    )

let () = Command.run cmd
