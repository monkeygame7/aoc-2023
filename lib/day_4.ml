let file = "inputs/day4"
let scan scanner fstring = Scanf.bscanf_opt scanner fstring (fun s -> s)

let rec contains value = function
  | [] -> false
  | h :: tl -> h = value || contains value tl
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
  aux ()
;;

let score_card scanner =
  let winners = read_winning_numbers scanner in
  Scanf.bscanf scanner " | " ();
  count_points winners scanner
;;

let rec inc_list count amount l =
  match count, l with
  | 0, l -> l
  | n, [] -> amount :: inc_list (n - 1) amount []
  | n, h :: tl -> (h + amount) :: inc_list (n - 1) amount tl
;;

let score scanner =
  let rec aux copies =
    match scan scanner "Card %d:" with
    | None -> 0
    | Some _ ->
      let num_cards, next_copies =
        match copies with
        | [] -> 1, []
        | h :: tl -> h + 1, tl
      in
      let num_matches = score_card scanner in
      num_cards + aux (inc_list num_matches num_cards next_copies)
  in
  aux []
;;

let run () = Scanf.Scanning.from_file file |> score |> Printf.printf "%d\n"
