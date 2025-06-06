open Stdio
open Str

type equation = {
  result: int;
  operands: int list;
}

let input_info = 
  let rec aux equation_list = 
    let line = In_channel.input_line In_channel.stdin in
    (match line with
    | None -> equation_list
    | Some x -> 
      let parts = split (regexp ":") x in
      match parts with
      | result :: operands -> 
        let result_int = int_of_string result in
        let operands = split (regexp " ") (List.hd operands) in
        let operands_int = List.map int_of_string operands in
        aux ({result = result_int; operands = operands_int} :: equation_list)
      | [] -> aux equation_list)
  in aux []
;;

let concat x y = (int_of_string ((string_of_int x)^(string_of_int y)));;
let is_equation_true equ =
  let nb_operators = (List.length equ.operands) - 1 in
  let rec aux idx accum l = 
    if idx == nb_operators then ((accum == equ.result), equ.result) else
    match l with
    | [] -> (false,equ.result)
    | hd :: tl -> (fst ((aux (idx+1) (accum + hd) tl)) ||
                   fst ((aux (idx+1) (accum * hd) tl)) ||
                   fst ((aux (idx+1) (concat accum hd) tl)), 
                   equ.result) in
  aux 0 (List.hd equ.operands) (List.tl equ.operands)
;;

let solve = 
  let are_equations_true = List.map is_equation_true input_info in
  let results = List.map (fun x -> if (fst x) then (snd x) else 0) are_equations_true in
  List.fold_left (+) 0 results;
;;

let () = printf "Total: %d\n" solve;;
