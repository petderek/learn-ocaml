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

(* I can't figure out how to recursive this. *)
let max = ref 0;;
let loop = ref 0;;
for i = 999 downto 1 do
    for j = 999 downto 1 do
        loop := i * j;
        max := if (isPalindrome !loop) && (!loop > !max) then !loop else !max;
    done;
done;

print_endline ("Problem 4: " ^ (string_of_int !max));;
