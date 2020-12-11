open! Core

module P = struct
  open Angstrom

  module P = struct
    module T = struct
      type t = { x : int; y : int } [@@deriving sexp, compare, hash]
    end

    include T
    include Comparable.Make (T)
  end

  include P
  open Let_syntax

  let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

  let word = take_while1 is_letter

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| Int.of_string

  type state = Occupied | Empty [@@deriving compare, equal]

  type spot = Seat of state | Floor

  let input =
    let empty = char 'L' >>| fun _ -> Seat Empty in
    let occupied = char '#' >>| fun _ -> Seat Occupied in
    let seat = empty <|> occupied in
    let floor = char '.' >>| fun _ -> Floor in
    let line = many1 (seat <|> floor) in
    let%bind input = sep_by1 end_of_line line in
    let tbl = Hashtbl.create (module P) in
    List.iteri input ~f:(fun y line ->
        List.iteri line ~f:(fun x c ->
            match c with
            | Seat s -> Hashtbl.set tbl ~key:{ x; y } ~data:s
            | Floor -> ()));
    return (tbl |> Hashtbl.to_alist |> Map.of_alist_exn)
end

let advance (m : (P.t, P.state, 'b) Map.t) =
  Map.mapi m ~f:(fun ~key:P.{ x; y } ~data ->
      let deltas =
        [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
      in
      let adjacent_occupied =
        List.fold deltas ~init:0 ~f:(fun prev (dx, dy) ->
            match Map.find m { x = x + dx; y = y + dy } with
            | None | Some Empty -> prev
            | Some Occupied -> prev + 1)
      in
      match data with
      | Empty when Int.(adjacent_occupied = 0) -> P.Occupied
      | Occupied when Int.(adjacent_occupied >= 4) -> P.Empty
      | v -> v)

let max_point m =
  Map.fold m
    ~init:P.{ x = 0; y = 0 }
    ~f:(fun ~key:P.{ x = x'; y = y' } ~data:_ { x; y } ->
      { x = max x x'; y = max y y' })

let state_to_string m =
  let s = ref [] in
  let add_char c = s := c :: !s in
  let P.{ x = width; y = height } = max_point m in
  for j = 0 to height do
    for i = 0 to width do
      match Map.find m { x = i; y = j } with
      | None -> add_char '.'
      | Some P.Occupied -> add_char '#'
      | Some Empty -> add_char 'L'
    done;
    add_char '\n'
  done;
  !s |> List.rev |> String.of_char_list

let rec fix f m equal =
  let m' = f m in
  if equal m' m then m' else fix f m' equal

let%expect_test "fix" =
  let s =
    "L.LL.LL.LL\n\
     LLLLLLL.LL\n\
     L.L.L..L..\n\
     LLLL.LL.LL\n\
     L.LL.LL.LL\n\
     L.LLLLL.LL\n\
     ..L.L.....\n\
     LLLLLLLLLL\n\
     L.LLLLLL.L\n\
     L.LLLLL.LL"
  in
  let m =
    Angstrom.(parse_string ~consume:Consume.All P.input s)
    |> Result.ok_or_failwith
  in
  let m' = fix advance m (Map.equal P.equal_state) in
  print_endline (state_to_string m');
  [%expect
    {|
    #.#L.L#.##
    #LLL#LL.L#
    L.#.L..#..
    #L##.##.L#
    #.#L.LL.LL
    #.#L#L#.##
    ..L.L.....
    #L#L##L#L#
    #.LLLLLL.L
    #.#L#L#.## |}]

let count_adjacent m (x,y) (width,height) =
  let deltas =
    [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
  in
  let rec iter_til_empty deltas ~f ~inc =
    match deltas with
    | [] -> ()
    | deltas ->
        let deltas = List.filter deltas ~f:(fun v -> f (snd v)) in
        iter_til_empty (List.map deltas ~f:inc) ~f ~inc
  in
  let adjacent_occupied = ref 0 in
  let () =
    iter_til_empty (List.map deltas ~f:(fun v -> (1,v)))
      ~inc:(fun (base, (x, y)) -> let base' = base + 1 in (base', (base' * (x/base), base' * (y /base))))
      ~f:(fun (dx, dy) ->
        let x = x + dx in
        let y = y + dy in
        match Map.find m P.{ x; y } with
        | _ when x >= width || y >= height || x < 0 || y < 0 -> false
        | None -> true
        | Some P.Empty -> false
        | Some Occupied ->
            incr adjacent_occupied;
            false)
  in
  !adjacent_occupied

let advance' (m : (P.t, P.state, 'b) Map.t) =
  let P.{ x = width; y = height } = max_point m in
  let width = width + 1 in
  let height = height + 1 in
  Map.mapi m ~f:(fun ~key:P.{ x; y } ~data ->
      let adjacent_occupied = count_adjacent m (x,y) (width,height) in
      match data with
      | Empty when Int.(adjacent_occupied = 0) -> P.Occupied
      | Occupied when Int.(adjacent_occupied >= 5) -> P.Empty
      | v -> v)

let%expect_test "fix2" =
  let s =
    "L.LL.LL.LL\n\
     LLLLLLL.LL\n\
     L.L.L..L..\n\
     LLLL.LL.LL\n\
     L.LL.LL.LL\n\
     L.LLLLL.LL\n\
     ..L.L.....\n\
     LLLLLLLLLL\n\
     L.LLLLLL.L\n\
     L.LLLLL.LL"
  in
  let m =
    Angstrom.(parse_string ~consume:Consume.All P.input s)
    |> Result.ok_or_failwith
  in
  let m' = fix advance' m (Map.equal P.equal_state) in
  print_endline (state_to_string m');
  [%expect
    {|
    #.L#.L#.L#
    #LLLLLL.LL
    L.L.L..#..
    ##L#.#L.L#
    L.L#.LL.L#
    #.LLLL#.LL
    ..#.L.....
    LLL###LLL#
    #.LLLLL#.L
    #.L#LL#.L# |}];
