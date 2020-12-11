open! Core
open! Async
open! Lib

let get_some = function Some v -> v | None -> assert false

let run input =
  let%bind reader = Reader.open_file input in
  let%bind ins =
    Angstrom_async.parse Angstrom.(sep_by1 end_of_line P.integer) reader
    >>| Result.ok_or_failwith
  in
  let arr = Array.of_list ins in
  let _, target = find_incorrect arr 25 |> Result.of_option ~error:"Not valid option" |> Result.ok_or_failwith in
  let rec finder low high =
    let sum = 
      let c = ref 0 in
      for i = low to high + 1 do
        c := !c + arr.(i)
      done; !c
    in 
    match () with
    | () when sum > target ->
      finder (low + 1) high
    | () when sum < target ->
      finder low (high + 1)
    | () -> (
      print_s [%message (sum : int)];
      arr.(low), arr.(high)
    )
  in 
  print_s [%message (target : int)];
  let low, high = finder 0 0 in
  print_s [%message (low : int) (high : int)];
  print_s [%message (low + high : int)];
  Reader.close reader

let cmd =
  Command.async ~summary:""
    Command.Let_syntax.(
      let%map_open input = anon ("filename" %: Filename.arg_type) in
      fun () -> run input)

let () = Command.run cmd
