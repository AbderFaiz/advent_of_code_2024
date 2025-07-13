open Stdio
open Str
module CharSet = Set.Make(Char)
module IntSet = Set.Make(Int)

let input_info = 
  let rec one_string inp nb_rows nb_columns = 
    let line = In_channel.input_line In_channel.stdin in
    match line with
    | None -> (inp, (nb_rows, nb_columns))
    | Some x -> one_string (inp ^ x) (nb_rows + 1) (String.length x)
  in one_string "" 0 0
;;

let input = (fst input_info);;
let nb_rows = (fst (snd input_info));;
let nb_columns =(snd (snd input_info));;

let encode_movement direction pos =
  match direction with
  | '^' -> let new_pos = pos - nb_columns in
           if (new_pos < 0) then -1 else new_pos
  | 'v' -> let new_pos = (pos + nb_columns) in
           if (new_pos >= (nb_rows*nb_columns)) then -1  else new_pos
  | '<' -> let new_pos = (pos - 1) in
           if (new_pos mod nb_columns == nb_columns -1) then -1 else
             (if (new_pos < 0) then -1 else new_pos)
  | '>' -> let new_pos = (pos + 1) in
           if (new_pos mod nb_columns == 0) then -1 else
             (if (new_pos >= (nb_rows*nb_columns)) then -1 else new_pos)
  | _ -> -1 (*Not supposed to happen*)
;;

let rec move_n direction pos n =
  if (n <= 0 || pos == -1) then pos
  else (move_n direction (encode_movement direction pos) (n-1))
  

let place_antinode_up delta_x delta_y pos =
  let pos_x = if (delta_x > 0) then (move_n '>' pos delta_x) else (move_n '<' pos (abs delta_x))  in
  let final_pos = if (pos_x == -1) then -1 else (move_n '^' pos_x delta_y) in
  final_pos
;;
  
let place_antinode_down delta_x delta_y pos =
  let pos_x = if (delta_x > 0) then (move_n '<' pos delta_x) else (move_n '>' pos (abs delta_x)) in
  let final_pos = if (pos_x == -1) then -1 else (move_n 'v' pos_x delta_y) in
  final_pos
;;    

let get_antennas =
    let rec aux pos accum=
    try
      let pos_antenna = search_forward (regexp {|[^.]|}) input pos in
      aux (pos_antenna + 1) (CharSet.add input.[pos_antenna] accum) 
    with Not_found -> accum
  in
  aux 0 CharSet.empty
;;

(* Get distance between two antennas
   requires pos1 < pos2;
*)
let distance pos1 pos2 =
  let y_pos1, x_pos1 = (pos1/nb_columns, pos1 mod nb_columns) in
  let y_pos2, x_pos2 = (pos2/nb_columns, pos2 mod nb_columns) in
  let x_diff, y_diff = (x_pos1 - x_pos2), (y_pos2 - y_pos1) in
  (x_diff, y_diff)
;;


let create_antinodes antenna fixed_antenna_pos =
  let rec aux pos accum =
    try
      let pos_antenna2 = search_forward (regexp (String.make 1 antenna)) input (pos + 1) in
      let x_diff, y_diff = (distance fixed_antenna_pos pos_antenna2) in
      let anti_node1 = (place_antinode_up x_diff y_diff fixed_antenna_pos) in
      let anti_node2 = (place_antinode_down x_diff y_diff pos_antenna2) in
      if (anti_node1 != -1) then
        (if (anti_node2 != -1) then
          (aux (pos_antenna2) (IntSet.add anti_node1 (IntSet.add anti_node2 accum)))
        else (aux (pos_antenna2) (IntSet.add anti_node1 accum)))
      else
        (if (anti_node2 != -1) then
          (aux (pos_antenna2) (IntSet.add anti_node2 accum))
        else
          (aux (pos_antenna2) accum))
    with Not_found -> accum
  in
  aux fixed_antenna_pos IntSet.empty
;;

let create_all_antinodes antenna =
  let rec aux pos accum =
  try
    let pos = search_forward (regexp (String.make 1 antenna)) input pos in
    let set_positions = create_antinodes antenna pos in
    aux (pos+1) (IntSet.union accum set_positions)
  with Not_found -> accum
  in
  aux 0 IntSet.empty
;;

let solve =
  let antinodes =
    CharSet.fold (fun c acc -> (IntSet.union (create_all_antinodes c) acc)) get_antennas IntSet.empty in
  IntSet.cardinal antinodes
;;

let () =
  printf "Total: %d\n" solve;;

