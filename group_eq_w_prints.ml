let rec group_eq_helper acc holder list =
  print_endline("holder: " ^ string_of_int holder);
  print_endline(" acc: " ^ string_of_int acc); 
  match list with
  | [] -> []
  (* | h :: [] -> print_endline("    after h list []");make_list_x_ntimes holder acc :: make_list_x_ntimes h 1 :: [] *)
  | h :: t -> 
    print_endline ("h: " ^ string_of_int h); 
    if( t = [] ) then 
      if ( h = holder ) then make_list_x_ntimes holder (acc+1) :: []
      else make_list_x_ntimes holder acc :: make_list_x_ntimes h 1 :: []
    else
    match print_endline(" h = holder: " ^ string_of_bool(h = holder)); (h = holder) with 
    | true -> group_eq_helper (acc + 1) holder t
    | false -> print_endline(" append list: "); (make_list_x_ntimes holder acc) :: group_eq_helper 1 h t 