(* Icy Hot

   Implement the function `reduce` which given

     l : a list of `temp`s

   returns a new list of `temp`s gotten by the following reduction rule:

   If `Hot i` and `Icy i` are adjacent (in particular, they must be
   carrying the same value) in any order, then they cancel out and are
   removed from the list.

   This rule should be carried out until it not possible to reduce the
   list any further.

   Examples:
   let _ = assert (reduce [Hot 0;Icy 0] = [])
   let _ = assert (reduce [Hot 0;Icy 1;Hot 1;Icy 0] = [])
   let _ = assert (reduce [Hot 0;Hot 1;Icy 2;Hot 2;Hot 0;Icy 1] = [Hot 0;Hot 1;Hot 0;Icy 1])

*)

type temp
  = Hot of int
  | Icy of int

let rec reduce (l : temp list) : temp list =
  
  let rec l_compare (tl : temp list) (acc : temp list) : temp list=
    match tl with
    | [] -> acc             (* if the temp list is empty return the front*)
    | h :: t ->
      match t with
      | [] -> (acc @ [h])
      | snd :: tt ->

        match (h,snd) with
        | (Hot n, Icy m) when n = m -> l_compare (acc @ tt) []
        | (Icy n, Hot m) when n = m -> l_compare (acc @ tt) []
        | _ -> l_compare (t) (acc @ [h])
  in
  
  match l with
  | [] -> []
  | _ -> l_compare l []



  let _ = assert (reduce [Hot 0;Icy 0] = [])
  let _ = assert (reduce [Hot 0;Icy 1;Hot 1;Icy 0] = [])
  let _ = assert (reduce [Hot 0;Hot 1;Icy 2;Hot 2;Hot 0;Icy 1] = [Hot 0;Hot 1;Hot 0;Icy 1])