(* Grouping Integers and Strings

   Implement a function `convert` which given

     l : a list of `int_or_string`s

   returns a list of `int_list_or_string_list`s such that adjacent
   `int`s and `string`s are grouped together.

   Example:
   

*)

type int_list_or_string_list
  = IntList of int list
  | StringList of string list

type int_or_string
  = Int of int
  | String of string

(* Go throughh the list
      encounter an int,
   go into rec function that will create a new sublist
      encounter a string
   go into rec function that will create a new sublist
     *)

let rec convert (l : int_or_string list) : int_list_or_string_list list =
  let rec loop (lst : int_or_string list) (i_acc : int list) (s_acc : string list) (acc : int_list_or_string_list list ): int_list_or_string_list list =
    match lst with
    | [] -> 
      if List.is_empty i_acc then
        acc @ [StringList (s_acc)]
      else
        acc @ [IntList (i_acc)]

    | h :: t ->
      if List.is_empty i_acc && List.is_empty s_acc then
        match h with 
        | Int n ->
          loop t (( n) :: i_acc) [] []
        | String s ->
          loop t [] (( s) :: s_acc) []
      

      else if List.is_empty i_acc then
        match h with 
        | Int n ->
          loop t [n] [] (acc @ [StringList (s_acc)])
        | String s ->
          loop t [] (s_acc @ [s]) (acc)
      
      else
        match h with 
        | Int n ->
          loop t (i_acc @ [n]) [] (acc)
        | String s ->
          loop t [] [s] (acc @ [IntList (i_acc)])
    in
    (loop l [] [] [])



let test_in = [Int 2; Int 3; String "a"; String "b"; Int 4; String "c"]
let test_out = [IntList [2;3]; StringList ["a";"b"]; IntList [4]; StringList ["c"]]

let test_in2 = [Int 2; Int 3; Int 4; Int 5; Int 6; Int 7;String "a"; String "b"; Int 4; String "c"; String "d"; String "e"; String "f"; String "g"; Int 3; Int 4; Int 5]

let _ = assert (convert test_in = test_out)
let x = convert test_in2