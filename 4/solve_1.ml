open Stdio

let next_char = function
| 'X' -> 'M'
| 'M' -> 'A'
| 'A' -> 'S'
| _ -> 'Z'
;;

let nb_columns = 
  let line = In_channel.input_line In_channel.stdin in
  match line with 
  | None -> 0
  | Some x -> String.length x 
;;

let nb_rows =
  let rec aux accum = 
    let line = In_channel.input_line In_channel.stdin in
    match line with 
    | None -> accum
    | Some _ -> aux (accum+1)
  in
  aux 1
;;

let () =  printf "rows : %d ; columns : %d\n" nb_rows nb_columns