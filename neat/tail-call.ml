(* Adapted from Euler Project #2 *)

let number = int_of_string (Array.get Sys.argv 1);;

let sum x =
  let rec sum i total = 
    if (i mod 3) = 0 || (i mod 5) = 0 then sum (i-1) (i+total)
    else if i > 0 then sum (i-1) total
    else total in
  sum x 0;;

print_endline ("sum (optimized): " ^ (string_of_int (sum number)));;