open Stdio
open Str

let next_char = function
| 'X' -> 'M'
| 'M' -> 'A'
| 'A' -> 'S'
| _ -> 'Z'
;;

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


let get_valid_indexes letter pos = 
  let filter i = 
  ( let right     = (if ((pos mod nb_columns) < (nb_columns - i)) then (pos + 1) else -99) in 
    let left      = (if ((pos mod nb_columns) > (i-1) ) then (pos -1) else -99) in
    let up        = (if ((pos - i*nb_columns) >= 0) then (pos - nb_columns) else -99) in
    let down      = (if ((pos + i*nb_columns) < nb_columns*nb_rows) then (pos + nb_columns) else -99) in
    let upright   = (let is_valid_upright = ((((pos - i*(nb_columns - 1)) mod nb_columns) - (pos mod nb_columns)) == i) in
                      if is_valid_upright then (up + 1) else -99) in
    let upleft    = (let is_valid_upleft = (((pos mod nb_columns) - ((pos - i*(nb_columns + 1)) mod nb_columns)) == i) in
                      if is_valid_upleft then (up - 1) else -99) in
    let downright = (let is_valid_downright = ((((pos + i*(nb_columns + 1)) mod nb_columns) - (pos mod nb_columns)) == i) in
                      if is_valid_downright then (down + 1) else -99) in
    let downleft  = (let is_valid_downleft = (((pos mod nb_columns) - ((pos + i*(nb_columns - 1)) mod nb_columns)) == i) in
                      if is_valid_downleft then (down - 1) else -99) in
    List.filter (fun x -> (snd x) >= 0) [('r',right);('l',left);('u',up);
                                   ('d',down);('R',upright);('L',upleft);
                                   ('\\',downright);('/',downleft)]) in
  (match letter with
  | 'X'     -> filter 3
  | 'M'     -> filter 2
  | 'A'     -> filter 1
  | 'S' | _ -> [])
;;

let rec count_xmas letter pos direction accum= 
  if letter == 'S' then (accum + 1) else
  let list_indexes = get_valid_indexes letter pos in
  let direction_filter = if direction != 'a' then List.filter (fun x -> (fst x) == direction) list_indexes else list_indexes in
  let adjacent_wanted = List.filter (fun l -> input.[snd l] == (next_char letter)) direction_filter in
  let rec aux = (function
                | (d, i):: tl -> count_xmas input.[i] i d accum + aux tl
                | [] -> accum)
  in aux adjacent_wanted
;;

let solve = 
  let rec aux pos accum=
    try
      let pos_x = search_forward (regexp "X") input pos in
      aux (pos_x + 1) (accum + count_xmas 'X' pos_x 'a' 0 )
    with Not_found -> accum
  in
    aux 0 0
;;
let () =  printf "Total : %d\n" solve