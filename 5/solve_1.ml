open Stdio
open Str

let read_input =
  let rec aux rules updates= 
    let line = In_channel.input_line In_channel.stdin in
    match line with 
    | None -> rules, updates
    | Some x -> if (Str.string_match (regexp {|[0-9]+|[0-9]+|}) x 0) then
                  let z = (List.map int_of_string (Str.split (regexp {|||}) x)) in
                  aux ((List.nth z 0, List.nth z 1)::rules) updates else 
                (if (Str.string_match (regexp {|[0-9]+\|,|}) x 0) then 
                  aux rules ((List.map int_of_string (Str.split (regexp {|,|}) x))::updates) else aux rules updates)
  in
  aux [] []
;;

let rules = (fst read_input)
let updates = (snd read_input)

let () = List.iter (fun x -> printf "(%d, %d)\n" (fst x) (snd x)) rules
