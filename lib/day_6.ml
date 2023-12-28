let name = "inputs/day6"
let scan scanner fstring = Scanf.bscanf_opt scanner fstring (fun s -> s)

type game =
  { time : int
  ; dist : int
  }

let print_list l =
  let rec aux = function
    | [] -> ()
    | h :: tl ->
      print_int h;
      print_string " ";
      aux tl
  in
  aux l;
  print_newline ()
;;

let rec parse_numbers scanner =
  match scan scanner " %d " with
  | Some n -> n :: parse_numbers scanner
  | None -> []
;;

let parse_games scanner =
  Scanf.bscanf scanner "Time: " ();
  let times = parse_numbers scanner in
  Scanf.bscanf scanner "Distance: " ();
  let dists = parse_numbers scanner in
  List.map2 (fun time dist -> { time; dist }) times dists
;;

let estimate_time speed time = speed * time

let count_wins game =
  let rec aux game remaining_time =
    match remaining_time with
    | 0 -> 0
    | t ->
      let speed = game.time - t in
      let score = estimate_time speed t in
      let result = if score > game.dist then 1 else 0 in
      result + aux game (remaining_time - 1)
  in
  aux game game.time
;;

let solve_part_1 games =
  let rec aux = function
    | game :: games -> count_wins game :: aux games
    | [] -> []
  in
  aux games
;;

let solve_part_2 games =
  let game =
    List.fold_left
      (fun (time, dist) a -> time ^ string_of_int a.time, dist ^ string_of_int a.dist)
      ("", "")
      games
    |> fun (time, dist) -> { time = int_of_string time; dist = int_of_string dist }
  in
  solve_part_1 [ game ]
;;

let run () =
  Scanf.Scanning.from_file name
  |> parse_games
  |> solve_part_2
  |> List.fold_left ( * ) 1
  |> print_int;
  print_newline ()
;;
