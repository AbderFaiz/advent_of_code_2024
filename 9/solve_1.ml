open Stdio
open Str


let expand dm =
  let rec aux i acc=
    if (i >= String.length dm) then
      acc
    else (if (i == String.length dm -1)
         then
           let id = Char.chr (i/2 + int_of_char '0') in
           let file_size = (int_of_char dm.[i] - int_of_char '0') in
           let new_encoding = (String.make file_size id) in
           (acc ^ new_encoding)
         else
           let id = Char.chr (i/2 + int_of_char '0') in
           let file_size = (int_of_char dm.[i] - int_of_char '0') in
           let empty_space = (int_of_char dm.[i+1] - int_of_char '0') in
           let new_encoding = (String.make file_size id) ^ (String.make empty_space '.') in
           aux (i+2) (acc ^ new_encoding))
  in
  aux 0 String.empty
;;

let calculate_checksum dm=
  let rec aux id accum =
    if (dm.[id] == '.') then accum
    else
      let block = (int_of_char dm.[id] - int_of_char '0') in
      (aux (id+1) ((id * block) + accum))
  in
  aux 0 0;;
  
let solve = calculate_checksum "0099811188827773336446555566.............."
;;

let () =
  printf "%s\n" (expand "2333133121414131402");
  printf "Total: %d\n" solve;;

