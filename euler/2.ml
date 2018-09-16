(* By considering the terms in the Fibonacci sequence whose values do not
exceed four million, find the sum of the even-valued terms. *)
let sum = ref 0;;

let data = ref (1,2);;
while (fst !data) < 4_000_000 do
  if ((fst !data) mod 2) = 0 then sum := !sum + (fst !data);
  data := (snd !data), (snd !data) + (fst !data);
done;;

print_endline ("Problem 2: " ^ string_of_int !sum);;