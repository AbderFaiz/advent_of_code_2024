open Stdio
open Str


let rec construct_list l1 l2 =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> (l1, l2) 
  | Some x -> if (string_match (regexp {|\([0-9]+\) *\([0-9]+\)|}) x 0) then
               construct_list ((Float.to_int (Float.of_string (matched_group 1 x)))::l1)
                              ((Float.to_int (Float.of_string (matched_group 2 x)))::l2) 
              else 
                (l1, l2) 

let rec count_occurence x accum = function
| [] -> accum
| hd :: tl -> if (hd == x) then count_occurence x (accum + 1) tl else count_occurence x accum tl

let rec solve accum = function
  | (hd1::tl1, li2) -> let nb_occurences = count_occurence hd1 0 li2 in
                      solve (accum + (hd1 * nb_occurences)) (tl1, li2)
  | ([],_) -> accum

let () =
  let (li1,li2) = construct_list [] [] in
    printf "Total: %d\n" (solve 0 (li1,li2))