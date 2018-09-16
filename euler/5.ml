(* find the smallest positive number that is evenly divisible 
by all numbers from 1 to 20 *)

let bound = 10000000000;;

let div x y = (x mod y) = 0;;

let divisibleBy20 x =
    (div x 19)
    && (div x 17)
    && (div x 16)
    && (div x 13)
    && (div x 11)
    && (div x 9)
    && (div x 7);;

let rec loop i =
    if (divisibleBy20 i) then i
    else if i = bound then bound
    else loop (i+20);; 


print_endline ("Problem 5: " ^ (string_of_int (loop 20)));;