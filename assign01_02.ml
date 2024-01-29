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

let rec sum_helper (n:int) : int =
  if n == 1 then 
    1
  else
    (Int.div n 2) + (sum_helper (Int.div n 2))

let is_perfect (n : int) : bool =
  if n == (sum_helper n) then 
    true
  else
    false