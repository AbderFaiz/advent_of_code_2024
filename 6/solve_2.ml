open Stdio
open Str

let input_info = 
  let rec one_string inp nb_rows nb_columns = 
    let line = In_channel.input_line In_channel.stdin in
    match line with
    | None -> (inp, (nb_rows, nb_columns))
    | Some x -> one_string (inp ^ x) (nb_rows + 1) (String.length x)
  in one_string "" 0 0
;;

let input = fst input_info;;
let nb_rows = (fst (snd input_info));;
let nb_columns = (snd (snd input_info));;

let initial_pos = search_forward (regexp {|\^\|<\|>\|v|}) input 0;;
let guard_direction = input.[initial_pos];;

(*TODO handle exit from the sides*)
let rec is_run_a_loop new_map pos direction l_pos= 
  if (pos < 0 || pos >= (nb_rows*nb_columns)) then false else
  (if (List.exists (fun x -> ((fst x) == pos) && ((snd x) == direction)) (List.tl l_pos)) then 
    (List.iter (fun x -> printf "(%d,%c); " (fst x) (snd x)) l_pos;printf "%d %c\n" pos direction; true) else 
  match direction with
  | '^' -> let new_pos = (pos - nb_columns) in
           if (new_pos >= 0 && new_map.[new_pos] == '#') then is_run_a_loop new_map pos '>' l_pos  else is_run_a_loop new_map new_pos '^' ((new_pos,'^')::l_pos)
  | 'v' -> let new_pos = (pos + nb_columns) in
           if (new_pos < (nb_rows*nb_columns) && new_map.[new_pos] == '#') then is_run_a_loop new_map pos '<' l_pos else is_run_a_loop new_map new_pos 'v' ((new_pos,'v')::l_pos)
  | '<' -> let new_pos = (pos - 1) in
           if (new_pos >= 0 && new_map.[new_pos] == '#') then is_run_a_loop new_map pos '^' l_pos else is_run_a_loop new_map new_pos '<' ((new_pos,'<')::l_pos)
  | '>' -> let new_pos = (pos + 1) in
           if (new_pos < (nb_rows*nb_columns) && new_map.[new_pos] == '#') then is_run_a_loop new_map pos 'v' l_pos else is_run_a_loop new_map new_pos '>' ((new_pos,'>')::l_pos)
  | _ -> false) (*Not supposed to happen*)

let rec eliminate_duplicates = function
| a :: (b :: _ as t) -> if a = b then eliminate_duplicates t else a :: eliminate_duplicates t
| smaller -> smaller;;
 
let solve =
    let rec aux pos accum=
      printf " i(#) = %d\n" pos;
      if pos > (nb_columns*nb_rows) then accum else
      if (pos == initial_pos) then aux (pos+1) accum else 
      let map_update = String.mapi (fun i c -> if i = pos then '#' else c) input in
      if (is_run_a_loop map_update initial_pos guard_direction [(initial_pos, guard_direction)]) then aux (pos + 1) (accum + 1)
      else aux (pos + 1) accum
    in
    aux 0 0;;

let () = printf "Total: %d\n" solve;;
