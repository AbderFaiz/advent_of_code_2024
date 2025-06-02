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


let rec is_safe status tolerance prev_element l = 
if (tolerance < 0) then false else 
(match status with
| "ko"         -> (match l with
                  | a :: b :: tl -> (is_safe (monotony prev_element a) (tolerance - 1) 0 (prev_element::b::tl)) ||
                                    (is_safe (monotony a b) (tolerance - 1) 0 (b::tl))
                  | _ :: [] | [] -> true)
| "increasing" -> (match l with
          | a :: b :: tl -> (if ((monotony a b) == "increasing") then (is_safe "increasing" tolerance a (b::tl)) 
                            else ((is_safe "increasing" (tolerance-1) 0 (prev_element::b::tl)) || 
                                  (is_safe "increasing" (tolerance-1) 0 (a::tl))))
          | _ :: [] | [] -> true)
| "decreasing" -> (match l with
          | a :: b :: tl -> (if ((monotony a b) == "decreasing") then (is_safe  "decreasing" tolerance a (b::tl))
                            else ((is_safe "decreasing" (tolerance-1) 0 (prev_element::b::tl)) || 
                                  (is_safe "decreasing" (tolerance-1) 0 (a::tl))))
          | _ :: [] | [] -> true)
| "confirm_monotony"       -> (match l with
                                | a :: b :: tl -> let initial_monotonity = (monotony prev_element a) in 
                                                  let current_monotonity = (monotony a b) in 
                                                  let is_foul = (if initial_monotonity == current_monotonity then 0 else 1) in
                                                  (is_safe current_monotonity (tolerance - is_foul) a (a::tl)) ||
                                                  (is_safe initial_monotonity (tolerance - is_foul) a (prev_element::b::tl))
                                | _ :: [] | [] -> true)
| "init" -> (match l with
            | a :: b :: tl -> is_safe (if ((a == b) || (abs(a - b) >= 4)) then "ko" else "confirm_monotony") tolerance a (b::tl)
            | _ :: [] | [] -> true)
| _            -> true )
;; 


let rec solve accum = function
  | hd :: tl -> if (is_safe "init" 1 0 hd) then solve (accum + 1) tl else solve accum tl
  | [] -> accum
;;

let () =
  let li = construct_list [] in
    printf "Total: %d\n" (solve 0 li)