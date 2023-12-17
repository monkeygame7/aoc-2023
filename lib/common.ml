
let rec read_lines (ic : in_channel) : string list =
  match In_channel.input_line ic with
  | Some line -> line :: read_lines ic
  | None -> []

let read_file path =
  let ic = open_in path in
  read_lines ic


let rec for_each f l =
    match l with
    | [] -> ()
    | next :: rest ->
            f next;
            for_each f rest


let rec sum_list l =
    match l with
    | value :: rest -> value + sum_list rest
    | [] -> 0
