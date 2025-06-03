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


let get_valid_indexes pos = 
  let filter i = 
  ( let up        = (if ((pos - i*nb_columns) >= 0) then (pos - nb_columns) else -99) in
    let down      = (if ((pos + i*nb_columns) < nb_columns*nb_rows) then (pos + nb_columns) else -99) in
    let upright   = (let is_valid_upright = ((((pos - i*(nb_columns - 1)) mod nb_columns) - (pos mod nb_columns)) == i) in
                      if is_valid_upright then (up + 1) else -99) in
    let upleft    = (let is_valid_upleft = (((pos mod nb_columns) - ((pos - i*(nb_columns + 1)) mod nb_columns)) == i) in
                      if is_valid_upleft then (up - 1) else -99) in
    let downright = (let is_valid_downright = ((((pos + i*(nb_columns + 1)) mod nb_columns) - (pos mod nb_columns)) == i) in
                      if is_valid_downright then (down + 1) else -99) in
    let downleft  = (let is_valid_downleft = (((pos mod nb_columns) - ((pos + i*(nb_columns - 1)) mod nb_columns)) == i) in
                      if is_valid_downleft then (down - 1) else -99) in
    List.filter (fun x -> (snd x) >= 0) [('R',upright);('L',upleft);
                                         ('\\',downright);('/',downleft)]) in
  
    filter 1
;;

let valid_configs = [
    [('R','S');('L','M');('\\','S');('/','M')];
    [('R','M');('L','M');('\\','S');('/','S')];
    [('R','S');('L','S');('\\','M');('/','M')];
    [('R','M');('L','S');('\\','M');('/','S')];
  ]
;;

let count_xmas pos =
  let is_xmas pos = 
    (let list_indexes = List.map (fun x -> (fst x, input.[snd x])) (get_valid_indexes pos) in
    if ((List.length list_indexes) == 4) then 
      (List.exists (fun x -> (compare list_indexes x) == 0) valid_configs) else false) in
  if (is_xmas pos) then 1 else 0    
;;

let solve = 
  let rec aux pos accum=
    try
      let pos_a = search_forward (regexp "A") input pos in
      aux (pos_a + 1) (accum + count_xmas pos_a)
    with Not_found -> accum
  in
    aux 0 0
;;
let () =  printf "Total : %d\n" solve