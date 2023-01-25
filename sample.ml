let rec fact x = if x < 1 then 1 else x * fact (x - 1)

let _ = print_endline (string_of_int (fact 5))