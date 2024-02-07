(* Perfect numbers

   A positive integer `n` is perfect if it is equal to the sum of its
   proper divisors.

   Please implement the function `is_perfect` of type `int -> bool`
   which, given an positive integer `n`, returns `true` if `n` is
   perfect and `false` otherwise.

   Examples:
   let _ = assert (is_perfect 6)        (* 1 + 2 + 3 = 6 *)
   let _ = assert (is_perfect 28)       (* 1 + 2 + 4 + 7 + 14 = 28 *)
   let _ = assert (not (is_perfect 24)) (* 1 + 2 + 3 + 4 + 6 + 8 + 12 <> 24 *)

 *)

 let sum_of_proper_divisors (n : int) : int =
  let rec aux acc i =
    if i >= n then acc
    else if n mod i = 0 then aux (acc + i) (i + 1)
    else aux acc (i + 1)
  in
  if n > 0 then aux 0 1 else 0
;;

let is_perfect (n : int) : bool =
  n > 0 && (sum_of_proper_divisors n) = n
;;

let () =
  assert (is_perfect 6);
  assert (is_perfect 28);
  assert (not (is_perfect 24));
  assert (not (is_perfect 0));  