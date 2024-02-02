(* Taxicab numbers

   A taxicab number is a positive integer which can be expressed as
   the sum of two positive cubes in more than one way. (This is
   slightly different than the usual definition.)

   Please implement the function `taxicab` of type `int -> int` which,
   given a positive integer `n`, returns the the number of ways that
   `n` can be expressed as the sum of two positive cubes.

   Examples:
   let _ = assert (taxicab 2 = 1)
   let _ = assert (taxicab 5 = 0)
   let _ = assert (taxicab 1729 = 2)   (* 1729 = 1^3 + 12^3 = 3^3 + 9^3 *)
   let _ = assert (taxicab 4104 = 2)   (* 4104 = 2^3 + 16^3 = 9^3 + 15^3 *)

 *)

let taxicab (n : int) : int =
  let rec find_pairs a count =
    if a * a * a > n then
      count
    else
      let b_cubed = n - (a * a * a) in
      let b = int_of_float (Float.pow (float_of_int b_cubed) (1. /. 3.)) in
      if a * a * a + b * b * b = n && a <= b then
        find_pairs (a + 1) (count + 1)
      else
        find_pairs (a + 1) count
  in
  find_pairs 1 0


  let _ = assert (taxicab 2 = 1)
  let _ = assert (taxicab 5 = 0)
  let _ = assert (taxicab 1729 = 2)   (* 1729 = 1^3 + 12^3 = 3^3 + 9^3 *)
  let _ = assert (taxicab 4104 = 2)   (* 4104 = 2^3 + 16^3 = 9^3 + 15^3 *)