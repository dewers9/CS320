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

let rec convert (l : int_or_string list) : int_list_or_string_list list =
  let rec go (lst : int_or_string list) (i_acc : int list) (s_acc : string list) (acc : int_list_or_string_list list ): int_list_or_string_list list =
    match lst with
    | [] -> assert false
      (* if List.is_empty i_acc then
        acc @ [StringList (s_acc)]
      else
        acc @ [IntList (i_acc)] *)

    | h :: t ->
      if List.is_empty i_acc && List.is_empty s_acc then
        match h with 
        | Int n ->
          go t [n] [] []
        | String s ->
          go t [] [s] []
      

      else if List.is_empty i_acc then
        match h with 
        | Int n ->
          go t [n] [] (acc @ [StringList (s_acc)])
        | String s ->
          go t [] (s_acc @ [s]) (acc)
      
      else
        match h with 
        | Int n ->
          go t (i_acc @ [n]) [] (acc)
        | String s ->
          go t [] [s] (acc @ [IntList (i_acc)])
    in
  go l [] [] []

(* let test_in = [String "call"; String "doll"; String "ere"; Int 200; Int 3; String "a"; String "b"; Int 4; String "call"; String "doll"; String "ere"]
let test_out = [StringList ["call";"doll";"ere"]; IntList [200;3]; StringList ["a";"b"]; IntList [4]; StringList ["call";"doll";"ere"]]
let _ = assert (convert test_in = test_out) *)