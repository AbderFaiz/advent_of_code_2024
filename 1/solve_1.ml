open Stdio
open Str


let rec construct_list l1 l2 =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> (List.sort compare l1, List.sort compare l2) 
  | Some x -> if (string_match (regexp {|\([0-9]+\) *\([0-9]+\)|}) x 0) then
               construct_list ((Float.of_string (matched_group 1 x))::l1)
                              ((Float.of_string (matched_group 2 x))::l2) 
              else 
                (List.sort compare l1, List.sort compare l2) 

let rec solve accum = function
  | (x::tl1, y::tl2) -> solve (accum + (abs 
                                          (int_of_float (x -.y))
                                        )) (tl1, tl2)
  | ([],[]) | (_,[]) | ([],_) -> accum

let () =
  let (li1,li2) = construct_list [] [] in
    printf "Total: %d\n" (solve 0 (li1,li2))