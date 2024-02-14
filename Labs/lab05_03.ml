(* Matrix-vector multiplication

   Note to TF/TAs and myself: if you have time, go over how to deal
   with errors using options or results.

   Implement the function `mv_mul` which, given

     a : a list of list of floats representing a matrix (as a list of rows)
     v : a list of floats representing a vector

   returns the produce of `a` and `v`.  You may assume that `a` is
   well-formed, and that the multiplication is well-define (i.e., `v`
   has as many entries as `a` does columns.

*)

type 'a matrix = 'a list list

let rec mv_mul (a : float matrix) (v : float list) : float list =
  let rec mul_across (l : float list) (value : float) : float list =
    match l with
    | [] -> []
    | lh :: lt -> lh *. val :: (mul_across lt value)
  in
  
  match v with
  | [] -> []
  | vh :: vt ->  
    match a with
    | [] -> []
    | ah :: at -> (mul_across ah vh) :: (mv_mul at vt)