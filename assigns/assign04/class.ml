let rec max lt l curr_max =
  match l with
  | [] -> curr_max
  | h :: t -> 
    if (lt h) && (h>curr_max) then 
      max lt t h
    else
      max lt t curr_max

let maximum leq l =
  List.fold_left (fun leq x y -> if if leq x y then y else x)

  let max leq x y = if leq x y then y else x
  
