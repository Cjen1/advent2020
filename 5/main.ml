open! Core
open! Async
open! Lib

let get_some = function Some v -> v | None -> assert false

let rec fold_left_2 ~f ~init l =
  match l with
  | a :: (b :: _ as rem) -> fold_left_2 ~f ~init:(f init a b) rem
  | remainder -> (init, remainder)

let run input =
  let%bind reader = Reader.open_file input in
  let%bind seats = Angstrom_async.parse Angstrom.(sep_by1 end_of_line P.data) reader >>| Result.ok_or_failwith in
  let seats = List.map seats ~f:Lib.parse_row_col in
  (* p1 *)
  let _res = List.max_elt seats ~compare:Int.compare |> get_some in
  (* P2 *)
  let seats = List.sort seats ~compare:Int.compare in
  let res, _ = fold_left_2 seats ~init:None ~f:(fun acc low high ->
      match acc with
      | Some v -> Some v
      | None when high - low = 2 -> Some (high - 1)
      | None when high - low > 2 -> failwiths ~here:[%here] "Too large of a jump" (high, low) [%sexp_of : (int * int)]
      | None when high - low < 1 -> failwiths ~here:[%here] "Too small of a jump" (high, low) [%sexp_of : (int * int)]
      | None -> None
    )
  in 
  print_s [%message (res : int option)];
  Reader.close reader

let cmd =
  Command.async ~summary:""
    Command.Let_syntax.(
      let%map_open input = anon ("filename" %: Filename.arg_type) in
      fun () -> run input)

let () = Command.run cmd
