(* Find the sum of all multiples of 3 or 5 below 100 *)
let sum = ref 0;;

let checkMultiple = fun y x -> (x mod y) = 0;;
let mult3 = checkMultiple 3;;
let mult5 = checkMultiple 5;;

for i = 1 to 999 do
  if mult3 i || mult5 i then sum :=  !sum + i
done;;

print_endline ("Problem 1: " ^ string_of_int !sum);;