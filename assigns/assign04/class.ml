(* let rec max lt l curr_max =
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
   *)


let num_digits (n : int) : int = 
  let rec helper n2 =
    if n2 <= 10 then
      1
    else
      1 + (helper (n2/10))
  in
  helper n

type animal =
| Cow of {name: string; age: int}
| Chicken of {name: string; birth_year: int}
| Pig of int
| Goose

let get_name_and_age (a : animal ) ( year : int) =
  match a with
  | Cow { name = n ; age = i } -> ( Some n, Some i)
  | Chicken info ->
  ( Some info .name , Some ( year - info . birth_year ))
  | Pig age -> (None , Some age)
  | Goose -> (None , None )

let rec foo l =
  match l with
  | [] -> []
  | false :: bs ->
  List .map (fun x -> x - 1) (0 :: foo bs)
  | true :: bs -> bar l
  and bar l =
  match l with
  | [] -> []
  | false :: bs -> foo l
  | true :: bs -> List .map ((+) 1) (0 :: bar bs)

let bitonic (l : int list) : bool =
  let rec is_inc prev l2 : bool =
    match l2 with
    | [] -> true
    | h::t -> (prev < h) && (is_inc h t)
  in
  let rec is_dec prev l2 : bool =
    match l2 with
    | [] -> true
    | h::t -> (prev > h) && (is_dec h t)
  in
  let rec inc_then_dec prev l2 : bool =
    match l2 with
    | [] -> true
    | h::t -> 
        if (prev < h) then 
          inc_then_dec h t
        else
          is_dec prev l2
  in
  match l with
  | [] -> true
  | h :: t ->
    is_inc h t || is_dec h t || inc_then_dec h t


let _ = assert (bitonic [1;2;3;2;1] = true)
let _ = assert (bitonic [1;2;3] = true)
let _ = assert (bitonic [3;2;1;2] = false)
let _ = assert (bitonic [] = true)
let _ = assert (bitonic [1;1] = false)
let _ = assert (bitonic [1;2;1;2] = false) 


let op (accum : (int -> int)) (next : (int -> int))  = 
  fun x -> max (accum x) (next x)

let base n = max n 0

let func_max (fs : (int -> int) list ) : int -> int =
  List.fold_left op base fs
let _ = assert
( func_max [(+) 1; fun x -> x * x] 1 = 2)
let _ = assert
( func_max [(+) 1; fun x -> x * x] ( -2) = 4)