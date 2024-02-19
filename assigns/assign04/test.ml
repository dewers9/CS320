let rec ( -- ) i j = if i > j then [] else i :: i + 1 -- j

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


let make_tups (n : 'a) =

  1--n
  |> List.map snd
  |> List.flatten 
  |> List.map thd


  