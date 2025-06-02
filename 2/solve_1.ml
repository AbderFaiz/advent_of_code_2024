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


let rec is_safe status l =  match status with
| "ko"         -> false
| "increasing" -> (match l with
          | a :: b :: tl -> if ((monotony a b) == "increasing") then (is_safe "increasing" (b::tl)) else (is_safe "ko" [])
          | _ :: [] | [] -> is_safe "ok" [])
| "decreasing" -> (match l with
          | a :: b :: tl -> if ((monotony a b) == "decreasing") then (is_safe "decreasing" (b::tl)) else (is_safe "ko" [])
          | _ :: [] | [] -> is_safe "ok" [])
| "init"       -> (match l with
          | a :: b :: tl -> is_safe (monotony a b) (b::tl)
          | _ :: [] | [] -> is_safe "ok" [])
| _            -> true (*any other value is considered ok*)
;;

let rec solve accum = function
  | hd :: tl -> if (is_safe "init" hd) then solve (accum + 1) tl else solve accum tl
  | [] -> accum
;;

let () =
  let li = construct_list [] in
    printf "Total: %d\n" (solve 0 li)