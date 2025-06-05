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


let rec run_guard pos direction l_pos = 
  if (pos < 0 || pos >= (nb_rows*nb_columns)) then (List.tl l_pos) else
  match direction with
  | '^' -> let new_pos = (pos - nb_columns) in
           if (new_pos >= 0 && input.[new_pos] == '#') then run_guard pos '>' l_pos else run_guard new_pos '^' (new_pos::l_pos)
  | 'v' -> let new_pos = (pos + nb_columns) in
           if (new_pos < (nb_rows*nb_columns) && input.[new_pos] == '#') then run_guard pos '<' l_pos  else run_guard new_pos 'v' (new_pos::l_pos)
  | '<' -> let new_pos = (pos - 1) in
           if (new_pos mod nb_columns == nb_columns -1) then (List.tl l_pos) else
           if (new_pos >= 0 && input.[new_pos] == '#') then run_guard pos '^' l_pos else run_guard new_pos '<' (new_pos::l_pos)
  | '>' -> let new_pos = (pos + 1) in
           if (new_pos mod nb_columns == 0) then (List.tl l_pos) else 
           if (new_pos < (nb_rows*nb_columns) && input.[new_pos] == '#') then run_guard pos 'v' l_pos else run_guard new_pos '>' (new_pos::l_pos)
  | _ -> [] (*Not supposed to happen*)

let rec eliminate_duplicates = function
| a :: (b :: _ as t) -> if a = b then eliminate_duplicates t else a :: eliminate_duplicates t
| smaller -> smaller;;

let solve =
    let initial_pos = search_forward (regexp {|\^\|<\|>\|v|}) input 0 in
    let guard_direction = input.[initial_pos] in
    let list_of_positions = (List.sort compare (run_guard initial_pos guard_direction [initial_pos]))in
  List.length (eliminate_duplicates list_of_positions);;

let () = printf "Total: %d\n" solve;;
