open Stdio

let input =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> ""
  | Some x -> x
;;

type file_block = {
  id: int;
  size: int;
  empty_space : int;
  can_move: bool;
  }

let rec print_disk_map = function
  | [] -> ()
  | x::tl -> (printf "(%d %d %d %b) - " x.id x.size x.empty_space x.can_move; print_disk_map tl)

let expand dm =
  let rec aux i acc=
    if (i >= String.length dm) then
      acc
    else (if (i == String.length dm -1)
         then
           let file_id = (i/2) in
           let file_size = (int_of_char dm.[i] - int_of_char '0') in
           let new_block = {id = file_id; size = file_size; empty_space=0; can_move = (file_id != 0)} in
           new_block :: acc
         else
           let file_id = (i/2) in
           let file_size = (int_of_char dm.[i] - int_of_char '0') in
           let empty_spc = (int_of_char dm.[i+1] - int_of_char '0') in
           let new_block = {id = file_id; size = file_size; empty_space=empty_spc; can_move = (file_id != 0)} in
           aux (i+2) (new_block :: acc))
  in
  aux 0 []
;;

let reverse_list = List.rev;;

let fill_empty_space file1 file2 gain_space =
  let new_ept_space = if (gain_space) then file1.empty_space - file2.size + file2.empty_space + file2.size
                      else file1.empty_space - file2.size
  in
  let n_file1 = {id = file1.id; size = file1.size; empty_space = 0; can_move=file1.can_move} in
  let n_file2 = {id = file2.id; size = file2.size; empty_space = new_ept_space; can_move = false} in
  n_file1::[n_file2]
;;

let custom_findindex f l =
  let index = List.find_index f l in
  match index with
  | None -> -1
  | Some i -> i
;;  

let cant_move f =
  {id=f.id; size=f.size; empty_space=f.empty_space; can_move=false};;

let add_empty_space i1 i2 lst=
  if (i1 == i2) then []
  else
    let imp_f = List.nth lst i1 in
    let mov_f = List.nth lst i2 in
    [{id=imp_f.id;
      size=imp_f.size;
      empty_space=imp_f.empty_space+mov_f.size+mov_f.empty_space;
      can_move=imp_f.can_move}];;

let sublist n j lst =
  List.take (j - n) (List.drop n lst)
;;

let compact reversed_dm =
  let ordered_dm = (reverse_list reversed_dm) in
  let size_dm = List.length ordered_dm in
  let rec aux dm =
    (* printf "\n -------------- \n"; *)
    (* print_disk_map dm; *)
    if (List.for_all (fun f -> f.can_move == false) dm) then dm
    else
      let reverse_dm = (reverse_list dm) in
      let cur_file_to_move_index = (size_dm - (custom_findindex (fun f -> f.can_move == true) reverse_dm) - 1) in
      let cur_file_to_move = (List.nth dm cur_file_to_move_index) in
      let new_index = (custom_findindex (fun f -> f.empty_space >= cur_file_to_move.size) dm) in
      let new_dm =
        (if (new_index == -1 || new_index >= cur_file_to_move_index) then
          (List.take cur_file_to_move_index dm)@[(cant_move cur_file_to_move)]@(List.drop (cur_file_to_move_index + 1) dm)
        else
          let impacted_file = List.nth dm new_index in
          let is_impacted_file_space =
            if (cur_file_to_move_index - new_index == 1) then cur_file_to_move_index else (cur_file_to_move_index - 1) in
          (List.take new_index dm)@
            (fill_empty_space impacted_file cur_file_to_move (cur_file_to_move_index == new_index + 1))@
              (sublist (new_index+1) (is_impacted_file_space) dm)@
                (add_empty_space is_impacted_file_space cur_file_to_move_index dm)@
                  (List.drop (cur_file_to_move_index+1) dm)
        )
          
          
      in
      aux new_dm
  in
  aux ordered_dm 
;;

let calculate_file_block_cs file_block id=
  let rec aux acc counter=
    if (counter == file_block.size) then acc
    else aux (file_block.id*(id+counter) + acc) (counter+1)
  in
  aux 0 0
;;

let calculate_checksum dm=
  let rec aux id accum =
    function
    | [] -> accum
    | x::tl -> aux (id+x.size+x.empty_space) ((calculate_file_block_cs x id)+accum) tl
  in
  aux 0 0 dm;;
  
let solve =
  let dm = expand input in
  calculate_checksum (compact dm);
;;

let () =
  printf "Total: %d\n" solve;;
