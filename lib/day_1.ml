open Common

let get_calibration nums =
  let first_digit = List.nth nums 0 in
  let last_digit = List.nth nums (List.length nums - 1) in
  (first_digit * 10) + last_digit
;;

let rec sum_list l =
  match l with
  | i :: rest -> i + sum_list rest
  | [] -> 0
;;

let rec to_digit_list text =
  if String.length text = 0
  then []
  else (
    let first_char = String.get text 0 in
    let next = String.sub text 1 (String.length text - 1) in
    match first_char with
    | '0' .. '9' -> int_of_string (String.make 1 first_char) :: to_digit_list next
    | _ ->
      if String.starts_with ~prefix:"one" text
      then 1 :: to_digit_list next
      else if String.starts_with ~prefix:"two" text
      then 2 :: to_digit_list next
      else if String.starts_with ~prefix:"three" text
      then 3 :: to_digit_list next
      else if String.starts_with ~prefix:"four" text
      then 4 :: to_digit_list next
      else if String.starts_with ~prefix:"five" text
      then 5 :: to_digit_list next
      else if String.starts_with ~prefix:"six" text
      then 6 :: to_digit_list next
      else if String.starts_with ~prefix:"seven" text
      then 7 :: to_digit_list next
      else if String.starts_with ~prefix:"eight" text
      then 8 :: to_digit_list next
      else if String.starts_with ~prefix:"nine" text
      then 9 :: to_digit_list next
      else if String.starts_with ~prefix:"zero" text
      then 0 :: to_digit_list next
      else to_digit_list next)
;;

let run () =
  read_file "inputs/day1"
  |> List.map to_digit_list
  |> List.map get_calibration
  |> sum_list
  |> string_of_int
;;
