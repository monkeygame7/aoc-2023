open Common

let file = "inputs/day7"
let card_priority_part_1 = String.index "23456789TJQKA"
let card_priority_part_2 = String.index "J23456789TQKA"

type card =
  { symbol : char
  ; rank : int
  }

let new_card ranker symbol = { symbol; rank = ranker symbol }

module Card = struct
  type t = card

  let compare a b = a.rank - b.rank
end

type hand =
  { cards : card list
  ; rank : int
  ; bid : int
  }

module CardMap = Map.Make (Card)
module CardSet = Set.Make (Card)
module IntMap = Map.Make (Int)

let contains key count = IntMap.exists (fun k _ -> k = key) count
let print_card_and_rank c = Printf.printf "%c(%d)" c.symbol c.rank
let print_card c = Printf.printf "%c" c.symbol

let print_list print_item l =
  let rec aux = function
    | i :: [] -> print_item i
    | i :: tl ->
      print_item i;
      print_string ", ";
      aux tl
    | [] -> ()
  in
  print_char '[';
  aux l;
  print_char ']'
;;

let print_map_items print_key print_val items =
  let rec aux = function
    | (k, v) :: [] ->
      print_key k;
      print_string ": ";
      print_val v
    | (k, v) :: tl ->
      print_key k;
      print_string ": ";
      print_val v;
      print_string ", ";
      aux tl
    | [] -> ()
  in
  print_char '{';
  aux items;
  print_char '}'
;;

let compare_cards (a : card) (b : card) = a.rank - b.rank

let compare_hands (a : hand) (b : hand) =
  match a.rank - b.rank with
  | 0 ->
    List.map2 compare_cards a.cards b.cards
    |> List.find_opt (fun x -> x != 0)
    |> Option.value ~default:0
  | n -> n
;;

let count_types cards =
  let rec aux acc = function
    | c :: tl ->
      let current_count =
        match CardMap.find_opt c acc with
        | Some x -> x
        | None -> 0
      in
      aux (CardMap.add c (current_count + 1) acc) tl
    | [] -> acc
  in
  let card_map = aux CardMap.empty cards in
  let count_map =
    CardMap.to_list card_map
    |> List.fold_left
         (fun acc (card, count) ->
           let value =
             match IntMap.find_opt count acc with
             | Some value -> card :: value
             | None -> [ card ]
           in
           IntMap.add count value acc)
         IntMap.empty
  in
  card_map, count_map
;;

let get_hand_rank_part_1 cards =
  let _, of_a_kind = count_types cards in
  if contains 5 of_a_kind
  then 7 (*five of a kind*)
  else if contains 4 of_a_kind
  then 6 (*four of a kind*)
  else if contains 3 of_a_kind
  then if contains 2 of_a_kind then 5 (*full house*) else 4 (*three of a kind*)
  else (
    match IntMap.find_opt 2 of_a_kind |> Option.map List.length with
    | Some 2 -> 3 (*two pair*)
    | Some 1 -> 2 (*single pair*)
    | None -> 1 (*high card*)
    | Some _ -> failwith "cannot have more than 2 pairs...")
;;

let get_hand_rank_part_2 (cards : card list) =
  let card_map, count_map = count_types cards in
  let num_wilds =
    CardMap.find_opt (new_card card_priority_part_2 'J') card_map
    |> Option.value ~default:0
  in
  let of_a_kind num_wilds target =
    IntMap.to_list count_map
    |> List.filter (fun (count, _) -> count <= target)
    |> List.map (fun (count, cards) -> List.map (fun card -> count, card) cards)
    |> List.flatten
    |> List.filter_map (fun (count, card) ->
      match card.symbol with
      | 'J' -> if count = target then Some count else None
      | _ ->
        let wilds_needed = target - count in
        if wilds_needed <= num_wilds then Some wilds_needed else None)
  in
  match of_a_kind num_wilds 5 with
  | _ :: _ -> 7
  | _ ->
    (match of_a_kind num_wilds 4 with
     | _ :: _ -> 6
     | _ ->
       (match of_a_kind num_wilds 3 with
        | wilds_3 :: _ ->
          (match of_a_kind num_wilds 2 with
           | wilds_2 :: _ when wilds_3 + wilds_2 <= num_wilds -> 5
           | _ -> 4)
        | _ ->
          (match of_a_kind num_wilds 2 with
           | wilds_1st :: wilds_2nd :: _ when wilds_1st + wilds_2nd <= num_wilds -> 3
           | _ :: _ -> 2
           | _ -> 1)))
;;

let parse_hand hand_ranker card_ranker scanner =
  let cards = scan scanner " %s" in
  match cards with
  | Some s when String.length s > 0 ->
    let cards =
      List.init (String.length s) (String.get s) |> List.map (new_card card_ranker)
    in
    let bid = Option.get (scan scanner " %d ") in
    let rank = hand_ranker cards in
    Some { cards; bid; rank }
  | _ -> None
;;

let rec parse_hands hand_ranker card_ranker scanner =
  match parse_hand hand_ranker card_ranker scanner with
  | Some hand -> hand :: parse_hands hand_ranker card_ranker scanner
  | None -> []
;;

let score_hand rank hand = rank * hand.bid

let solve hand_ranker card_ranker scanner =
  parse_hands hand_ranker card_ranker scanner
  |> List.sort compare_hands
  |> List.mapi (fun idx cards -> score_hand (idx + 1) cards)
  |> List.fold_left ( + ) 0
  |> print_int
  |> print_newline
;;

let solve_part_1 = solve get_hand_rank_part_1 card_priority_part_1
let solve_part_2 = solve get_hand_rank_part_2 card_priority_part_2
let run () = Scanf.Scanning.from_file file |> solve_part_2
