open Stdio
(* open Str *)


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

(* To be continued *)
let compact dm =
  let reversed = (reverse_list dm) in
  reversed 
;;

let calculate_checksum dm=
  let rec aux accum id = function
    | [] -> accum
    | x::tl -> (aux (id+1) ((id * x) + accum)) tl
  in
  aux 0 0 dm;;
  
let solve = 0
  (* let expanded = expand input in *)
  (* calculate_checksum *)
    (* (compact expanded (reverse_compact expanded)); *)
;;

let () =
  let dm = expand input in
  List.iter (printf "%d") (dm);
  printf "\n ----------------- \n";
  List.iter (printf "%d") (reverse_list dm);
  (* printf "%s\n" (reverse_compact (expand input)); *)
  (* printf "%s\n" (compact (expand input) (reverse_compact (expand input))); *)
  (* printf "Total: %d\n" solve;; *)

