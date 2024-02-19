(* For Loops

   In Python we might write the following code to generate Pythagorean
   triples.

     def pythagoraean_triples(n):
         out = []
         for i in range(1, n):
             for j in range(j + 1, n):
                 for k in range(k + 1, n):
                     if i * i + j * j == k * k:
                         out.append((i, j, k))
         return out

   In this problem we will be implementing a version of this logic in
   OCaml using higher-order functions.  This problem has two parts:

   ====================

   1. Implement a function `for_loop` which, given

     l : 'a list
     f : a function of type ('a -> 'b list)

   returns the result of applying `f` to each element of the `l` and
   then concatenating the outputs.  For example, `for_loop [x;y;z] f`
   is equivalent to `f x @ f y @ f z`.

   Hint: Take a look at the `List.concat` in the standard library.

   ====================

   2. Implement a function `pythagorean_triples` which, given

     n : an integer

   returns the list of all triples (i, j, k) in lexicographical order
   such that

   - i * i + j * j = k * k
   - i < j < k < n

   You should use the `for_loop` function and mimic the structure of
   the above Python code.  See the example `test` below for how to use
   the `for_loop` function like a for-loop in Python.

   Think about how you might use `List.filter` to get the Pythagorean
   triples from the the list of all triples.

   Example:
   let _ = assert (pythagorean_triples 20 =
     [(3, 4, 5); (5, 12, 13); (6, 8, 10); (8, 15, 17); (9, 12, 15)])
*)

let rec range i j =
  if i >= j then
    []
  else
    i :: range (i + 1) j

let rec for_loop (l : 'a list) (f : 'a -> 'b list) : 'b list =
  match l with
  | [] -> []
  | h :: t -> (f h) @ (for_loop t f)

let make_tups (n : 'a) =

  1--n
  |> List.map snd
  |> List.flatten 
  |> List.map thd
  |> List.flatten 

  
  
let rec snd (j : 'a) : ('a list) list=
  let rec loop (j : 'a) (k : 'a) : ('a list) list =
    match j with 
    | 1 -> [[j;k]]
    | j -> [j;k] :: (loop (j-1) k)
  in
  loop j j

let rec thd (jk : 'a list) =
  let rec loop (i : 'a) (j : 'a) (k : 'a)=
    match i with
    | 1 -> [(1,j,k)]
    | i -> (i,j,k) :: (loop (i-1) (j) (k))
  in
  match jk with
  | j::k::[] -> 
    loop j j k
  | _ -> assert false
    

let filter_helper (t : (int * int * int)) : bool =
  match t with
  | i,j,k -> i<j && j<k && i*i + j*j = k*k

let foo i j =
  for_loop (range i j) (fun k ->
      [k + k])

(*
   foo is like the Python code:

   out = []
   for k in range(i, j):
       out.append(k + k)
   return out
*)

(* let _ = assert (foo 1 10 = List.map (fun k -> k + k) (range 1 10)) *)
let sort_tuples_by_first_element (lst : ('a * 'a * 'a) list) =
  let compare_first_elem (x1, _, _) (x2, _, _) = compare x1 x2 in
  List.sort compare_first_elem lst
  
let rec unique l acc =
  match l with
  | [] -> List.rev acc
  | h :: t -> 
    if List.mem h acc then
      (unique t acc)
    else
      (unique t (h :: acc))

let pythagorean_triples (n : int) : (int * int * int) list =
  let get_all n =
    for_loop (range 1 n) make_tups
    |> List.filter filter_helper
  in
  sort_tuples_by_first_element (unique (get_all n) [])

let _ = assert (pythagorean_triples 20 =
[(3, 4, 5); (5, 12, 13); (6, 8, 10); (8, 15, 17); (9, 12, 15)])