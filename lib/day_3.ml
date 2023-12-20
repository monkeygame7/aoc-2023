let file = "inputs/day3"

type position =
  { row : int
  ; col : int
  ; len : int
  }

type int_token =
  { value : int
  ; pos : position
  }

type symbol_token =
  { symbol : char
  ; pos : position
  }

type token =
  | Int of int_token
  | Symbol of symbol_token

let print_int_token int_token =
  Printf.printf
    "Integer { value : %3d  row: %3d  col: %3d  len: %d }\n"
    int_token.value
    int_token.pos.row
    int_token.pos.col
    int_token.pos.len
;;

let print_symbol_token symbol_token =
  Printf.printf
    "Symbol  { symbol:   %c  row: %3d  col: %3d  len: %d }\n"
    symbol_token.symbol
    symbol_token.pos.row
    symbol_token.pos.col
    symbol_token.pos.len
;;

let print_tokens tokens =
  List.iter
    (fun t ->
      match t with
      | Int int_token -> print_int_token int_token
      | Symbol symbol_token -> print_symbol_token symbol_token)
    tokens;
  print_newline ();
  tokens
;;

let parse_int scan first_digit =
  let rec pow x n = if n <= 0 then 1 else x * pow x (n - 1) in
  Scanf.bscanf_opt scan "%n%d%n" (fun start_pos value end_pos ->
    let len = end_pos - start_pos in
    (first_digit * pow 10 len) + value, len)
  |> Option.value ~default:(first_digit, 0)
;;

let rec parse_lines scan row col =
  try
    Scanf.bscanf scan "%c" (fun c ->
      match c with
      | '\n' -> parse_lines scan (row + 1) 0
      | '.' -> parse_lines scan row (col + 1)
      | '0' .. '9' ->
        let value, len = parse_int scan (int_of_char c - int_of_char '0') in
        let len = len + 1 in
        Int { value; pos = { row; col; len } } :: parse_lines scan row (col + len)
      | _ ->
        Symbol { symbol = c; pos = { row; col; len = 1 } }
        :: parse_lines scan row (col + 1))
  with
  | End_of_file -> []
;;

let split_tokens tokens =
  let rec aux acc_ints acc_symbols = function
    | [] -> acc_ints, acc_symbols
    | Int i :: tl -> aux (i :: acc_ints) acc_symbols tl
    | Symbol c :: tl -> aux acc_ints (c :: acc_symbols) tl
  in
  aux [] [] tokens
;;

let is_adjacent (number : int_token) (symbol : symbol_token) =
  let row = symbol.pos.row in
  let col = symbol.pos.col in
  let min_row = number.pos.row - 1 in
  let max_row = number.pos.row + 1 in
  let min_col = number.pos.col - 1 in
  let max_col = number.pos.col + number.pos.len in
  min_row <= row && row <= max_row && min_col <= col && col <= max_col
;;

let is_adjacent_to_symbol (number : int_token) (symbols : symbol_token list) =
  let result = symbols |> List.filter (is_adjacent number) |> List.is_empty |> not in
  if result
  then (* let _ = print_int_token number in *)
    result
  else (* let _ = print_int_token number in *)
    result
;;

let part1 (numbers : int_token list) (symbols : symbol_token list) =
  let rec aux (acc : int_token list) = function
    | [] -> acc
    | num :: tl when is_adjacent_to_symbol num symbols -> aux (num :: acc) tl
    | _ :: tl -> aux acc tl
  in
  aux [] numbers
;;

let gear_ratio numbers symbol =
  match symbol.symbol with
  | '*' ->
    (match List.filter (fun n -> is_adjacent n symbol) numbers with
     | [ n1; n2 ] -> n1.value * n2.value
     | _ -> 0)
  | _ -> 0
;;

let part2 (numbers : int_token list) (symbols : symbol_token list) =
  symbols |> List.map (gear_ratio numbers)
;;

let run () =
  open_in file
  |> Scanf.Scanning.from_channel
  |> fun file ->
  parse_lines file 0 0
  (* |> print_tokens *)
  |> split_tokens
  |> fun (ints, symbols) ->
  part2 ints symbols
  (* |> List.map (fun t -> t.value) *)
  |> Common.sum_list
  |> Printf.printf "%d\n"
;;
