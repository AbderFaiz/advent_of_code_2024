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
let input_mutable = Bytes.of_string input;;

let nb_rows = (fst (snd input_info));;
let nb_columns = (snd (snd input_info));;

type coords = {
  position  : int;
  direction : char;
}

module CoordsOrd : Set.OrderedType with type t = coords = struct
  type t = coords
  let compare p1 p2 =
    match Int.compare p1.position p2.position with
    | 0 -> Char.compare p1.direction p2.direction
    | c -> c
end

module CoordsSet = Set.Make(CoordsOrd)
let initial_pos = search_forward (regexp {|\^\|<\|>\|v|}) input 0;;
let guard_direction = input.[initial_pos];;

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

let potential_obstacles = eliminate_duplicates (List.sort compare (run_guard initial_pos guard_direction [initial_pos]));;

let rec is_run_a_loop new_map pos direction visited_states_set= 
  if (pos < 0 || pos >= (nb_rows*nb_columns)) then false else
  (if (CoordsSet.mem {position = pos; direction = direction} visited_states_set) then true else
  match direction with
  | '^' -> let new_pos = (pos - nb_columns) in
           if (new_pos >= 0 && (Bytes.get new_map new_pos) == '#') then 
            is_run_a_loop new_map pos '>' (visited_states_set |> CoordsSet.add {position = pos; direction = '^'}) else 
            is_run_a_loop new_map new_pos '^' (visited_states_set |> CoordsSet.add {position = pos; direction = '^'})
  | 'v' -> let new_pos = (pos + nb_columns) in
           if (new_pos < (nb_rows*nb_columns) && (Bytes.get new_map new_pos) == '#') then
            is_run_a_loop new_map pos '<' (visited_states_set |> CoordsSet.add {position = pos; direction = 'v'})else
            is_run_a_loop new_map new_pos 'v' (visited_states_set |> CoordsSet.add {position = pos; direction = 'v'})
  | '<' -> let new_pos = (pos - 1) in
           if (new_pos mod nb_columns == nb_columns -1) then false else
           if (new_pos >= 0 && (Bytes.get new_map new_pos) == '#') then
            is_run_a_loop new_map pos '^' (visited_states_set |> CoordsSet.add {position = pos; direction = '<'}) else
            is_run_a_loop new_map new_pos '<' (visited_states_set |> CoordsSet.add {position = pos; direction = '<'})
  | '>' -> let new_pos = (pos + 1) in
           if (new_pos mod nb_columns == 0) then false else 
           if (new_pos < (nb_rows*nb_columns) && (Bytes.get new_map new_pos) == '#') then
            is_run_a_loop new_map pos 'v' (visited_states_set |> CoordsSet.add {position = pos; direction = '>'}) else
            is_run_a_loop new_map new_pos '>' (visited_states_set |> CoordsSet.add {position = pos; direction = '>'})
  | _ -> false) (*Not supposed to happen*)
 
let solve =
    let aux accum pos=
      if pos > (nb_columns*nb_rows) then accum else
      if (pos == initial_pos) then accum else 
      (Bytes.set input_mutable pos '#';
      if (is_run_a_loop input_mutable initial_pos guard_direction (CoordsSet.empty)) 
      then (Bytes.set input_mutable pos '.';(accum + 1))
      else (Bytes.set input_mutable pos '.';accum))
    in
    List.fold_left aux 0 potential_obstacles ;;

let () = printf "Total: %d\n" solve;;
