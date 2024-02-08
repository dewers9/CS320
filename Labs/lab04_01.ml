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

   Note to TA/TFs and myself: work through how would approach this problem

 *)

let taxicab (n : int) : int = assert false (* TODO *)

let rec loop i j =
  let cube_i = i * i * i in
  let cube_j = j * j * j in
  if cube_i + cube_j = n then
    1 + loop (i+1) (i+1)
  else if cube_i + cube_j > n then
    loop (i+1) (i+1)
  if cube_i > n then 
    0  
  else
    loop i (j + 1)
  in

loop 1 1
(* Nested for loop to recursive loop *)

let _ = assert (taxicab 1729 = 2)   (* 1729 = 1^3 + 12^3 = 3^3 + 9^3 *)
