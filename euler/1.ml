(* Find the sum of all multiples of 3 or 5 below 1000 *)

let checkMultiple y x = (x mod y) = 0;;
let mult3 = checkMultiple 3;;
let mult5 = checkMultiple 5;;

let rec sum x =
  if mult3 x || mult5 x then x + sum (x-1)
  else if x > 0 then sum (x-1)
  else 0;;

print_endline ("Problem 1: " ^ string_of_int (sum 999));;