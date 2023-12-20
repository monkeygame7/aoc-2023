let file = "inputs/day4"
let scan scanner fstring = Scanf.bscanf_opt scanner fstring (fun s -> s)

let rec contains value = function
  | [] -> false
  | h :: tl -> h = value || contains value tl
;;

let rec pow n = function
  | n when n < 0 -> 0
  | 0 -> 1
  | exp -> n * pow n (exp - 1)
;;

let read_winning_numbers scanner =
  let rec aux () =
    match scan scanner " %d " with
    | Some n -> n :: aux ()
    | None -> []
  in
  aux ()
;;

let count_points winners scanner =
  let rec aux () =
    match scan scanner " %d " with
    | None -> 0
    | Some n when contains n winners -> 1 + aux ()
    | Some _ -> aux ()
  in
  let num_matches = aux () in
  pow 2 (num_matches - 1)
;;

let score_card scanner =
  let winners = read_winning_numbers scanner in
  Scanf.bscanf scanner " | " ();
  count_points winners scanner
;;

let rec score scanner =
  match scan scanner "Card %d:" with
  | None -> 0
  | Some _ -> score scanner + score_card scanner
;;

let run () = Scanf.Scanning.from_file file |> score |> Printf.printf "%d\n"
