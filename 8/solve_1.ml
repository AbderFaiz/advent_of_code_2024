open Stdio
(* open Str *)

let input_info = 
  let rec one_string inp nb_rows nb_columns = 
    let line = In_channel.input_line In_channel.stdin in
    match line with
    | None -> (inp, (nb_rows, nb_columns))
    | Some x -> one_string (inp ^ x) (nb_rows + 1) (String.length x)
  in one_string "" 0 0
;;

let grid = (fst input_info);;
let nb_rows = (fst (snd input_info));;
let nb_columns =(snd (snd input_info));;

let solve = 0
;;

let () = printf "%d %d\n\n%s" nb_rows nb_columns grid;
printf "Total: %d\n" solve;;
         
