let rev_concat (ls : 'a list list) : 'a list =
  let rec conc (a : 'a list) (b: 'a list) =
    match b with
    | [] -> a
    | h :: t -> conc (revl (h :: (revl a [])) []) t
  
  and revll (l : 'a list list) (acc : 'a list list)=
    match l with
    | [] -> acc
    | h :: t -> revll t ((revl h []) :: acc)
  and 
  
  revl (l : 'a list) (acc : 'a list) =
    match l with
    | [] -> acc
    | h :: t -> revl t (h::acc)
  and
  
  merge (l : 'a list list) (acc : 'a list) =
    match l with
    | [] -> acc
    | h :: t -> merge t (conc acc h)
  in
  merge (revll ls []) [] 
