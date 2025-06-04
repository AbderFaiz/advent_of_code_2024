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

let rules_graph = (fst read_input)
let updates = (snd read_input)

let neighbors a = 
  let aux g a =
    let edge l (b, c) = if b = a then (c,'a') :: l
                        else if c = a then (b,'b') :: l
                        else l in
    List.fold_left edge [] g in
  aux rules_graph a
;;

let rec get_order nei = function
| (n,o)::tl -> if (nei == n) then o else get_order nei tl
| [] -> 'e'
;;

let rec is_page_ordered neighbors_p = function
| [] -> true
| hd :: tl -> if ((get_order hd neighbors_p) == 'a') then 
                is_page_ordered neighbors_p tl else false
;;   

let get_middle_page u = let u_len = List.length u in List.nth u (u_len/2);;

let sort u =
  let rec aux sorted_u u visited = (match visited with
  | [] -> sorted_u
  | hd::tl -> let taint_order = List.map (fun x -> (x, get_order x (neighbors hd))) sorted_u in
              let correct_pages = (List.map fst (List.filter (fun x -> (snd x) == 'a') taint_order)) in
              let uncorrect_pages = (List.map fst (List.filter (fun x -> (snd x) != 'a') taint_order)) in
              aux (uncorrect_pages@correct_pages) u tl)
  in
  aux u u u;;

let solve =
  let rec is_valid_update u = 
    match u with
    | hd :: tl -> 
      (let hd_neighbors = neighbors hd in
        if (is_page_ordered hd_neighbors tl) then (is_valid_update tl) else false
      )
    | [] -> true
  in
  let rec aux accum = function
  | [] -> accum
  | hd::tl -> if (is_valid_update hd) then aux accum tl
                                      else aux (accum + get_middle_page (sort hd)) tl in
  aux 0 updates
;;


let () = printf "Total: %d\n" solve
