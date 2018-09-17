(* Adapted from Euler Project #2 *)

let number = int_of_string (Array.get Sys.argv 1);;

let rec othersum i =
  if (i mod 3) = 0 || (i mod 5) = 0 then i + othersum (i-1)
  else if i > 0 then othersum (i-1)
  else 0;;

print_endline ("sum: " ^ (string_of_int (othersum number)));;