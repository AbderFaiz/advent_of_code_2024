open Stdio
open Str

let parse_mult_instruction line=
  let r = regexp {|mul( *\([0-9]+\) *, *\([0-9]+\) *)|} in 
  let rec aux pos accum=
    try
      let _ = search_forward r line pos in
      let x = float_of_string (matched_group 1 line) in
      let y = float_of_string (matched_group 2 line) in
      aux (match_end ()) (accum + int_of_float (x *. y))
    with Not_found -> accum
  in
    aux 0 0
;;
let rec solve accum =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> accum
  | Some x -> solve (accum + parse_mult_instruction x)
;;

let () =  printf "Total: %d\n" (solve 0)