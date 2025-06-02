open Stdio
open Str

let get_instructions_byregexp re line =
  let rec aux pos list_positions =
    try
      let start_pos = search_forward re line pos in
      aux (match_end ()) (list_positions@[start_pos])
    with Not_found -> list_positions
  in
  aux 0 []
;;

let active_regions x =
  let list_dos_positions = get_instructions_byregexp (regexp {|do()|}) x in
  let list_donts_positions = get_instructions_byregexp (regexp {|don't()|}) x in
  let rec aux do_flag l_do l_dont pos res = 
    if do_flag then 
      (match l_do with
      | hd::tl -> if (hd >= pos) then (aux false tl l_dont hd (res@[hd])) else (aux true tl l_dont pos res)
      | [] -> res)  
    else 
      (match l_dont with
      | hd::tl -> if (hd >= pos) then (aux true l_do tl hd (res@[hd])) else (aux false l_do tl pos res)
      | [] -> res)
  in
  aux false list_dos_positions list_donts_positions 0 [0]
;;

let rec is_active_mul mul_pos = function
| a :: b :: tl -> (if (a<= mul_pos) then
                     (if (mul_pos <= b) then true 
                   else is_active_mul mul_pos tl)
                  else false)
| [a] -> mul_pos >= a
| []  -> false


let parse_mult_instruction line=
  let active_regions_list = active_regions line in
  let r = regexp {|mul( *\([0-9][0-9]?[0-9]?\) *, *\([0-9][0-9]?[0-9]?\) *)|} in 
  let rec aux pos accum=
    try
      let mul_pos = search_forward r line pos in
      if (is_active_mul mul_pos active_regions_list) then
        (let x = float_of_string (matched_group 1 line) in
         let y = float_of_string (matched_group 2 line) in
         aux (match_end ()) (accum + int_of_float (x *. y)))
      else
         aux (match_end ()) accum
    with Not_found -> accum
  in
    aux 0 0
;;

let solve =
  let rec one_string input = 
    let line = In_channel.input_line In_channel.stdin in
    match line with
    | None -> input
    | Some x -> one_string (input ^ x)
  in 
  parse_mult_instruction (one_string "")
;;

let () =  printf "Total: %d\n" (solve)