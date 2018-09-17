(* 
    Find the difference between the sum of the squares of the first one hundred
    natural numbers and the square of the sum.
*)
(*385
3025
2640*)
let sumOfSquares x =
    let rec sum i total = match i with
        | 0 -> total
        | _ -> sum (i-1) (total + i * i) in
    sum x 0;;

let squareOfTheSum x = 
    let rec sum i total = match i with
        | 0 -> total
        | _ -> sum (i-1) (total + i) in
    let summed = sum x 0 in
    summed * summed;;

print_endline ("Problem 6: " ^ (string_of_int ((squareOfTheSum 10) - (sumOfSquares 100))));;