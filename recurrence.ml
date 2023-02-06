let rec count_times_ran_helper num acc =
  if num == 0 then acc
  else count_times_ran_helper (num-2) (acc+num)

let count_times_ran num = count_times_ran_helper num 0

let test = count_times_ran 10000000