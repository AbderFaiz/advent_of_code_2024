open Stdio
open Str


let rec construct_list l1=
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> l1
  | Some x -> let report = List.map int_of_string (Str.split (regexp {| |}) x) in
              construct_list (report::l1)
;;

let monotony a b = if ((abs (a - b)) < 4) then 
  (if (a < b) then "increasing" else 
  (if (a > b) then "decreasing" else "ko"))
else "ko"
;;


let rec is_safe status tolerance l = 
(Printf.printf "%s %d\n" status tolerance); 
if (tolerance < 0) then false else 
(match status with
| "ko"         -> is_safe "init" (tolerance - 1) l
| "increasing" -> (match l with
          | a :: b :: tl -> if ((monotony a b) == "increasing") then (is_safe "increasing" tolerance (b::tl)) else (is_safe "increasing" (tolerance-1) (b::tl))
          | _ :: [] | [] -> true)
| "decreasing" -> (match l with
          | a :: b :: tl -> if ((monotony a b) == "decreasing") then (is_safe  "decreasing" tolerance (b::tl)) else (is_safe  "decreasing" (tolerance-1) (b::tl))
          | _ :: [] | [] -> true)
| "init"       -> (match l with
          | a :: b :: tl -> is_safe (monotony a b) tolerance (b::tl)
          | _ :: [] | [] -> true)
| _            -> true )
;; 


let rec solve accum = function
  | hd :: tl -> (Printf.printf "%d\n" (List.hd hd)); if (is_safe "init" 1 hd) then solve (accum + 1) tl else solve accum tl
  | [] -> accum
;;

let () =
  let li = construct_list [] in
    printf "Total: %d\n" (solve 0 li)