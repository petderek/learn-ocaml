(* Find the sum of all multiples of 3 or 5 below 1000 *)

let checkMultiple y x = (x mod y) = 0;;
let mult3 = checkMultiple 3;;
let mult5 = checkMultiple 5;;

let sum x =
  let rec sum i total = 
    if mult3 i || mult5 i then sum (i-1) (i+total)
    else if i > 0 then sum (i-1) total
    else total in
  sum x 0;;

print_endline ("Problem 1: " ^ string_of_int (sum 999));;