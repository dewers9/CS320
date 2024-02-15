(* Concatenation Lists

   A `concatlist` is a list whose constructors are based on
   concatenation instead of cons.  It has a constructor for the empty
   list (Nil), a single element list (Single) and the concatentation
   of two lists (Concat).

   Implement a function `sort` which given

     l : a `concatlist`

   returns a regular list with the same element as `l` but in sorted
   order.  You should do this WITHOUT trying to first converting `l`
   into a regular list.  In particular, you CANNOT use the function
   `List.sort`.

   Example:
   let l = Concat (Concat (Single 3, Single 2), Concat (Single 1, Single 10))
   let _ = assert (sort l = [1;2;3;10])

*)

type 'a concatlist
  = Nil
  | Single of 'a
  | Concat of 'a concatlist * 'a concatlist

let sort (l : 'a concatlist) : 'a list =
  
  let rec sort_cons a b =
    match a with
    
    | Single m1, Single n1 ->

  let rec helper (lis : 'a concatlist) (acc : 'a list) : 'a list =
    (* Pull head *)
    match lis with 
    | Nil -> acc
    | Concat(h,t) ->
      (* Check if the head is a Concat or a Single *)
      match h with ->
      | Single m -> 
        (* If single get second *)
        match t with
        | Nil -> (acc :: [m])
        | Concat(th,tt) -> 
          match th ->
          (* Check if the second is a Concat or a Single *)
          | Single n ->
            if n > m then
              helper tt (acc :: [m]) :: [n] 
          | Concat n->
              


      | Concat m -> 
      

    (* Check if the second is a Concat or a Single *)
    


(* 

if is_Concat then
  open and find min


*)
    
    
      
