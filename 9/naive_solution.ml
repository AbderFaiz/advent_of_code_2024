open Stdio
open Str


let input =
  let rec one_string inp= 
    let line = In_channel.input_line In_channel.stdin in
    match line with
    | None -> inp
    | Some x -> one_string (inp ^ x)
  in one_string ""
;;

let repeat_string str n =
  String.concat "" (List.init n (fun _ -> str))
;;

let expand dm =
  let rec aux i acc=
    if (i >= String.length dm) then
      acc
    else (if (i == String.length dm -1)
         then
           let id = string_of_int (i/2) in
           let file_size = (int_of_char dm.[i] - int_of_char '0') in
           let new_encoding = (repeat_string id file_size) in
           (acc ^ new_encoding)
         else
           let id = string_of_int (i/2) in
           let file_size = (int_of_char dm.[i] - int_of_char '0') in
           let empty_space = (int_of_char dm.[i+1] - int_of_char '0') in
           let new_encoding = (repeat_string id file_size) ^ (String.make empty_space '.') in
           aux (i+2) (acc ^ new_encoding))
  in
  aux 0 String.empty
;;

let reverse_compact dm=
  let rec aux left_most_pos accum =
    try
      if (left_most_pos < 0) then accum
      else
        (let nleft_most_pos = search_backward (regexp "[0-9]") dm left_most_pos in
         aux (nleft_most_pos-1) (accum^(String.make 1 dm.[nleft_most_pos])))
    with Not_found -> accum
  in
  aux (String.length dm) (String.empty)
;;

let compact dm rcomp=
  let dm_len = (String.length rcomp) in
  let rec aux i pos accum =
    try
      let npos = search_forward (regexp {|\.|}) dm pos in
      aux (i+1) (npos+1) (accum ^ (String.sub dm pos (npos - pos)) ^ (String.make 1 rcomp.[i]))
    with Not_found -> (String.sub accum 0 dm_len) ^ (String.make 1 '.')
  in
  aux 0 0 String.empty 
;;

let calculate_checksum dm=
  let rec aux id accum =
    if (dm.[id] == '.') then accum
    else
      let block = (int_of_char dm.[id] - int_of_char '0') in
      (aux (id+1) ((id * block) + accum))
  in
  aux 0 0;;
  
let solve =
  let expanded = expand input in
  calculate_checksum
    (compact expanded (reverse_compact expanded));
;;

let () =
  printf "%s\n" (expand input);
  (* printf "%s\n" (reverse_compact (expand input)); *)
  (* printf "%s\n" (compact (expand input) (reverse_compact (expand input))); *)
  (* printf "Total: %d\n" solve;; *)

