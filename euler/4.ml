(* Find the largest palindrome made from the product of two 3-digit numbers. *)
(* 9009 = 91 × 99. *)

let endsMatch x =
    (String.get x 0) = (String.get x ((String.length x)-1));;

let shave x = String.sub x 1 ((String.length x)-2);;

let rec isPalindromeStr x = 
    (String.length x) = 0 
    || (String.length x) = 1
    || (String.length x) = 2 && (endsMatch x)
    || (endsMatch x) && isPalindromeStr (shave x);;

let isPalindrome x = isPalindromeStr (string_of_int x);;



print_endline (string_of_bool (isPalindrome 1));;
print_endline (string_of_bool (isPalindrome 111));;
print_endline (string_of_bool (isPalindrome 202));;
print_endline (string_of_bool (isPalindrome 12321));;
print_endline (string_of_bool (isPalindrome 345));;
print_endline (string_of_bool (isPalindrome 1231));;
print_endline (string_of_bool (isPalindrome 123421));;
