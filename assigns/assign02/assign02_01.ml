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
  assert false

let test_in = [String "call"; String "doll"; String "ere"; Int 2; Int 3; String "a"; String "b"; Int 4; String "call"; String "doll"; String "ere"]
let test_out = [StringList ["call";"doll";"ere"]; IntList [2;3]; StringList ["a";"b"]; IntList [4]; StringList ["call";"doll";"ere"]]
let _ = assert (convert test_in = test_out)