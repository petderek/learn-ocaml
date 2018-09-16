(* Find the largest palindrome made from the product of two 3-digit numbers. *)
let max a b = if a > b then a else b;;

let endsMatch x = (String.get x 0) = (String.get x ((String.length x)-1));;

let shave x = String.sub x 1 ((String.length x)-2);;

let rec isPalindromeStr x = match (String.length x) with
    | 0 -> true
    | 1 -> true
    | 2 -> (endsMatch x)
    | _ -> (endsMatch x) && isPalindromeStr (shave x);;

let isPalindrome x = isPalindromeStr (string_of_int x);;

let rec findPalindromes i = 
    let rec loop j = 
        if isPalindrome (i*j) then (i*j) (*biggest j for given i*)
        else if j = 0 then 0
        else loop(j-1) in
    if i = 0 then 0 else
    let trial = loop i in
    let next = findPalindromes(i-1) in
    if trial > next then trial else next;;

print_endline ("Problem 4: " ^ (string_of_int (findPalindromes 999)));;
