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
  let rec aux accum p =
    let pos_x = if (delta_x > 0) then (move_n '>' p delta_x) else (move_n '<' p (abs delta_x))  in
    if (pos_x == -1) then accum
    else
    (let final_pos = (move_n '^' pos_x delta_y) in
    if (final_pos == -1) then accum else (aux (final_pos::accum) final_pos))
  in
  aux [] pos
;;

let place_antinode_down delta_x delta_y pos =
  let rec aux accum p =
    let pos_x = if (delta_x > 0) then (move_n '<' p delta_x) else (move_n '>' p (abs delta_x))  in
    if (pos_x == -1) then accum
    else
    (let final_pos = (move_n 'v' pos_x delta_y) in
    if (final_pos == -1) then accum else (aux (final_pos::accum) final_pos))
  in
  aux [] pos
;;

let get_antennas =
  let rec aux pos accum =
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
      let antenna_as_antinode_set = (IntSet.add pos_antenna2 accum) in
      let set_antinode1 = List.fold_left (fun acc p -> IntSet.add p acc) IntSet.empty anti_node1 in
      let set_antinode2 = List.fold_left (fun acc p -> IntSet.add p acc) set_antinode1 anti_node2 in
      aux (pos_antenna2) (IntSet.union (IntSet.union set_antinode1 set_antinode2) antenna_as_antinode_set)
    with Not_found -> if ((IntSet.cardinal accum) == 1) then (IntSet.remove fixed_antenna_pos accum) else accum
  in
  aux fixed_antenna_pos (IntSet.add fixed_antenna_pos IntSet.empty)
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
  (* printf "\n\nAntinodes : \n"; *)
  (* IntSet.iter (fun d -> printf "%d " d) antinodes; *)
  IntSet.cardinal antinodes
;;

let () =
  (* printf "anitnodes for A\n"; *)
  (* IntSet.iter (fun p -> printf "%d " p) (create_antinodes 'A' 104); *)
  printf "Total: %d\n" solve;;

