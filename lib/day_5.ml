let name = "inputs/day5"
let scan scanner fstring = Scanf.bscanf_opt scanner fstring (fun s -> s)
let tracer = 47360831

type range =
  { start : int
  ; len : int
  }

let range_end src = src.start + src.len - 1

let format_range { start; len } =
  Printf.sprintf " %10d..%d (%d)" start (range_end { start; len }) len
;;

let range_of start end_ = { start; len = end_ - start + 1 }

type mapping_def =
  { dst_range : range
  ; src_range : range
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

let print_range range = Printf.printf "%s\n" (format_range range)

let print_ranges ranges =
  print_string "ranges:\n";
  List.sort (fun l r -> l.start - r.start) ranges |> List.iter print_range;
  print_newline ()
;;

let format_mapping { dst_range; src_range } =
  Printf.sprintf
    " %10d..%10d -> %10d..%10d"
    src_range.start
    (range_end src_range)
    dst_range.start
    (range_end dst_range)
;;

let print_mappings mappings =
  print_string "mappings:\n";
  let by_src l r = l.src_range.start - r.src_range.start in
  let by_dst l r = l.dst_range.start - r.dst_range.start in
  List.combine
    (List.sort by_src mappings |> List.map format_mapping)
    (List.sort by_dst mappings |> List.map format_mapping)
  |> List.iter (fun (l, r) ->
    print_string l;
    print_string "  |  ";
    print_endline r);
  print_newline ()
;;

let maybe_map_id src mapping =
  let pure_start = src - mapping.src_range.start in
  if pure_start < 0 || pure_start >= mapping.src_range.len
  then None
  else Some (mapping.dst_range.start + pure_start)
;;

let map_id mappings src =
  let mapped_id = List.nth_opt (List.filter_map (maybe_map_id src) mappings) 0 in
  match mapped_id with
  | None -> src
  | Some id -> id
;;

let merge l r =
  let start_dist = r.start - l.start in
  if start_dist <= l.len then Some (range_of l.start (range_end r)) else None
;;

let merge_ranges ranges =
  let rec aux = function
    | [] -> []
    | r :: [] -> [ r ]
    | l :: r :: tl ->
      (match merge l r with
       | Some merged -> aux (merged :: tl)
       | None -> l :: aux (r :: tl))
  in
  let ranges = List.sort (fun l r -> l.start - r.start) ranges in
  aux ranges
;;

let map_range { dst_range; src_range } { start; len } =
  let pure_start = start - src_range.start in
  { start = dst_range.start + pure_start; len }
;;

let get_overlap src dst =
  let src_end = range_end src in
  let dst_end = range_end dst in
  if src_end < dst.start || src.start > dst_end
  then None, [ src ]
  else (
    let overlap_start = max src.start dst.start in
    let overlap_len = min src_end dst_end - overlap_start + 1 in
    let overlap = { start = overlap_start; len = overlap_len } in
    let before = { start = src.start; len = overlap_start - src.start } in
    let after =
      { start = range_end overlap + 1; len = src_end - overlap_start - overlap.len + 1 }
    in
    Some overlap, List.filter (fun r -> r.len > 0) [ before; after ])
;;

let rec split_ranges = function
  | _, [] -> [], []
  | mapping, src :: ranges ->
    let mapped, unmapped = get_overlap src mapping.src_range in
    let mapped = Option.map (map_range mapping) mapped in
    let rest_mapped, rest_unmapped = split_ranges (mapping, ranges) in
    ( List.concat [ Option.to_list mapped; rest_mapped ]
    , List.concat [ unmapped; rest_unmapped ] )
;;

let rec map_ranges = function
  | [], [] -> []
  | [], ranges ->
    print_endline "some remaining ";
    ranges
  | mapping :: mappings, ranges ->
    let mapped, unmapped = split_ranges (mapping, ranges) in
    List.concat [ mapped; map_ranges (mappings, unmapped) ]
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
          let dst = List.nth row 0 in
          let start = List.nth row 1 in
          let len = List.nth row 2 in
          { dst_range = { start = dst; len }; src_range = { start; len } }))
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

let rec to_ranges = function
  | start :: len :: tl -> { start; len } :: to_ranges tl
  | [] -> []
  | _ -> failwith "expected only pairs"
;;

let solve_part_2 scanner =
  let rec aux tracer ranges =
    match parse_mappings scanner with
    | None ->
      Printf.printf "Final Tracer - %d\n" tracer;
      ranges
    | Some mappings ->
      Printf.printf "Tracer - %d\n" tracer;
      print_mappings mappings;
      print_endline "got ranges:";
      print_ranges ranges;
      let ranges = map_ranges (mappings, ranges) |> merge_ranges in
      print_endline "mapped to ranges:";
      print_ranges ranges;
      let tracer = map_id mappings tracer in
      aux tracer ranges
  in
  let seeds = List.nth (Option.get (parse_numbers scanner)) 0 in
  let seed_ranges = to_ranges seeds in
  aux tracer seed_ranges
  |> List.map (fun r -> r.start)
  |> List.fold_left Int.min Int.max_int
  |> print_int;
  print_newline ()
;;

let run () = Scanf.Scanning.from_file name |> solve_part_2
