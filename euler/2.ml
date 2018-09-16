(* By considering the terms in the Fibonacci sequence whose values do not
exceed four million, find the sum of the even-valued terms. *)

let count_evens x =
  let rec loop t =
    let first,second = t in
    if first > x then 0
    else if (first mod 2) = 0 then first + loop(second, (second+first))
    else loop(second, (second+first)) in
  loop (1,2);;

print_endline ("Problem 2: " ^ string_of_int (count_evens 4_000_000));;