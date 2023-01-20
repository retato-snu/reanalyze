(* let show_cmt (cmt_infos : CL.Cmt_format.cmt_infos) =
  let rec show_args (sl : string list) = 
    match sl with
    |[] -> ()
    |h::t -> (
      Printf.sprintf h;
     (show_args t)
     )
  in
  let rec show_cooments (comments) =
    match comments with
    |[] -> ()
    |h::t -> (
      Printf.sprintf fst h;
      Location.print_loc 
    )
  Printf.sprintf "cmt_modname : " ^ cmt_infos.cmt_modname;

  Printf.sprintf  "cmt_args : ";
  show_args(Array.to_list cmt_infos.cmt_args); *)
  

type var = integer | user_type

and integer = Integer of change

and change = {
  move_direction : direction;
  move_quantity : quantity;
}

and direction = Inc | Dec | Unknown

and quantity = int | Unknown

type user_type = {
  name : string;
  length : change;
  depth : change;
  sub : user_type list;
}

let exec_change change1 change2 = 
  if (change1.move_direction = change2.move_direction) then
    (match change1.quantity with
    |Unknown -> {move_direction = change1.move_direction; move_quantity = Unknown;}
    |_ -> (match change1.move_direction with
      |Unknown -> {move_direction = Unknown; move_quantity = Unknown;}
      |_ -> {move_direction = change1.move_quantity; move_quantity = (change1.move_quantity + change2.move_quantity);}
      ))
  else begin
    if change1.move_direction = Unknown
       then {move_direction = Unknown; move_quantity = Unknown;}
    else begin if change2.move_direction = Unknown
      then {move_direction = Unknown; move_quantity = Unknown;}
      else
        let amount = change1.move_quantity - change2.move_quantity in
        if amount > 0 then
          {move_direction = change1.move_direction; move_quantity = amount;}
        else
          {move_direction = change2.move_direction; move_quantity = -amount;}
    end
  end





    

let processCmt (cmt_infos : CL.Cmt_format.cmt_infos) =
  Read_cmt.print_info cmt_infos;
  match cmt_infos.cmt_annots with
  | Interface _ -> ()
  | Implementation structure -> ()
  | _ -> ()

let reportResults ~ppf = ()
