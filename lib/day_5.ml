let name = "inputs/day5"
let scan scanner fstring = Scanf.bscanf_opt scanner fstring (fun s -> s)

type mapping_def =
  { dst_start : int
  ; src_start : int
  ; len : int
  }

let print_list l =
  let rec aux = function
    | [] -> print_string "]"
    | i :: tl ->
      print_int i;
      (match tl with
       | [] -> ()
       | _ -> print_string ", ");
      aux tl
  in
  print_string "[";
  aux l;
  print_newline ()
;;

let maybe_map_item_id src mapping =
  let pure_start = src - mapping.src_start in
  if pure_start < 0 || pure_start >= mapping.len
  then None
  else Some (mapping.dst_start + pure_start)
;;

let map_id mappings src =
  let mapped_id = List.nth_opt (List.filter_map (maybe_map_item_id src) mappings) 0 in
  match mapped_id with
  | None -> src
  | Some id -> id
;;

let parse_number_row scanner =
  let rec aux () =
    match scan scanner "\n" with
    | Some _ -> []
    | None ->
      (match scan scanner " %d" with
       | Some id -> id :: aux ()
       | None -> [])
  in
  aux ()
;;

let parse_numbers scanner =
  let rec aux () =
    match parse_number_row scanner with
    | [] -> []
    | numbers -> numbers :: aux ()
  in
  Scanf.bscanf scanner " %s@: " (fun s -> if s = "" then None else Some (aux ()))
;;

let parse_mappings scanner =
  parse_numbers scanner
  |> Option.map
       (List.map (fun row ->
          { dst_start = List.nth row 0; src_start = List.nth row 1; len = List.nth row 2 }))
;;

let solve_part_1 scanner =
  let rec aux seeds =
    match parse_mappings scanner with
    | None -> seeds
    | Some mappings -> aux (List.map (map_id mappings) seeds)
  in
  let seeds = List.nth (Option.get (parse_numbers scanner)) 0 in
  aux seeds |> List.fold_left min Int.max_int
;;

let run () = Scanf.Scanning.from_file name |> solve_part_1 |> print_int |> print_newline
