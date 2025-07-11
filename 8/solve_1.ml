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
           if (new_pos < (nb_rows*nb_columns)) then -1  else new_pos
  | '<' -> let new_pos = (pos - 1) in
           if (new_pos mod nb_columns == nb_columns -1) then -1 else
           if (new_pos < 0) then -1 else new_pos
  | '>' -> let new_pos = (pos + 1) in
           if (new_pos mod nb_columns == 0) then -1 else 
           if (new_pos >= (nb_rows*nb_columns)) then -1 else new_pos
  | _ -> -1 (*Not supposed to happen*)
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

let solve = 0
;;

let () = printf "Antennas : ";
         CharSet.iter (fun a -> printf "%c " a) get_antennas;
         printf "\n";
printf "Total: %d\n" solve;;
         
