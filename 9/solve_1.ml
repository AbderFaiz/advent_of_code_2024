open Stdio

let input =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> ""
  | Some x -> x
;;

let expand dm =
  let rec aux i acc=
    if (i >= String.length dm) then
      acc
    else (if (i == String.length dm -1)
         then
           let id = (i/2) in
           let file_size = (int_of_char dm.[i] - int_of_char '0') in
           let new_encoding = List.init file_size (fun _ -> id) in
           new_encoding@acc
         else
           let id = (i/2) in
           let file_size = (int_of_char dm.[i] - int_of_char '0') in
           let empty_space = (int_of_char dm.[i+1] - int_of_char '0') in
           let new_encoding = (List.init empty_space (fun _ -> -1)) @ (List.init file_size (fun _ -> id)) in
           aux (i+2) (new_encoding@acc))
  in
  aux 0 []
;;

let reverse_list =
  let rec aux acc = function
    | [] -> acc
    | x :: tl -> aux (x::acc) tl
  in
  aux []
;;


let get_dm_and_queue =
  let rec aux acc queue = function
    | [] -> acc, (reverse_list queue)
    | -1 :: tl -> aux (-1::acc) queue tl
    | x :: tl -> aux (x::acc) (x::queue) tl
  in
  aux [] []
;;

let compact dm =
  let reversed, queue = (get_dm_and_queue dm) in
  let len_dm = (List.length queue) in
  let rec aux acc q l=
    if (List.length acc == len_dm) then (reverse_list acc) else
    match l with
    | [] -> (reverse_list acc)
    | -1::tl -> aux ((List.hd q)::acc) (List.tl q) tl
    | x :: tl -> aux (x::acc) q tl
  in
  aux [] queue reversed
;;

let calculate_checksum dm=
  let rec aux id accum = function
    | [] -> accum
    | x::tl -> aux (id+1) (id*x + accum) tl
  in
  aux 0 0 dm;;
  
let solve = 
  let dm = expand input in
  calculate_checksum (compact dm);
;;

let () =
  printf "Total: %d\n" solve;;

