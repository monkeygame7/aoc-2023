open Common

type cube_set =
  { mutable red : int
  ; mutable blue : int
  ; mutable green : int
  }

type game_result =
  { id : int
  ; reveals : cube_set list
  }

let apply_result set result =
  match String.split_on_char ' ' result with
  | [ i; "red" ] -> !set.red <- int_of_string i
  | [ i; "blue" ] -> !set.blue <- int_of_string i
  | [ i; "green" ] -> !set.green <- int_of_string i
  | _ -> raise (Invalid_argument result)
;;

let parse_cube_set reveal_text =
  let results = String.split_on_char ',' reveal_text |> List.map String.trim in
  let set = ref { red = 0; blue = 0; green = 0 } in
  for_each (apply_result set) results;
  !set
;;

let parse_game_reveals game_text =
  String.split_on_char ';' game_text |> List.map parse_cube_set
;;

let parse_line line =
  let id_text, game_text =
    match String.split_on_char ':' line with
    | [ id; game ] -> id, game
    | _ -> raise (Invalid_argument line)
  in
  let game_id =
    match String.split_on_char ' ' id_text with
    | [ "Game"; id ] -> int_of_string id
    | _ -> raise (Invalid_argument id_text)
  in
  let reveals = parse_game_reveals game_text in
  { id = game_id; reveals }
;;

let is_valid_game limit game =
  game.reveals
  |> List.for_all (fun reveal ->
    reveal.red <= limit.red && reveal.blue <= limit.blue && reveal.green <= limit.green)
;;

let min_set game =
  let set = { red = 0; blue = 0; green = 0 } in
  for_each
    (fun reveal ->
      set.red <- max reveal.red set.red;
      set.blue <- max reveal.blue set.blue;
      set.green <- max reveal.green set.green)
    game.reveals;
  set
;;

let run () =
  read_file "inputs/day2"
  |> List.map parse_line
  |> List.map min_set
  |> List.map (fun set -> set.red * set.blue * set.green)
  |> sum_list
  |> string_of_int
;;
