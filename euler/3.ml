(* What is the largest prime factor of the number 600851475143? *)
let getMax = fun x y -> if x > y then x else y;;

let rec isPrime x y =
    if y = 1 then true else if x mod y = 0 then false else isPrime x (y-1);;

let prime = fun x -> isPrime x (x-1);;

let rec biggestPrime x =
    let rec loop i = 
        if x = i then x
        else if prime i && x mod i = 0 then getMax i (biggestPrime (x / i))
        else loop (i+1) in
    loop(2);;

print_endline ("Problem 3: " ^ string_of_int (biggestPrime 600851475143));;