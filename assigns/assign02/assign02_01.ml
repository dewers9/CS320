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
  let rec append_to_int_list (lst : int_or_string list) (acc : int list) (list_list : int_list_or_string_list list) : int_list_or_string_list list=
    match lst with
    | [] -> (List.rev ((IntList (List.rev acc)) :: list_list)) (* Return accumulated IntList when the input list is empty *)
    | Int i :: rest -> append_to_int_list rest (i :: acc) list_list (* Append Int to accumulator and continue recursion *)
    | String _ :: _ -> append_to_str_list lst [] ((IntList (List.rev acc)) :: list_list) (* Stop recursion and return StringList when a String is encountered *)
  and append_to_str_list (lst : int_or_string list) (acc : string list) (list_list : int_list_or_string_list list) : int_list_or_string_list list =
    match lst with
    | [] -> (List.rev ((StringList (List.rev acc)) :: list_list)) (* Return accumulated IntList when the input list is empty *)
    | String i :: rest -> append_to_str_list rest (i :: acc) list_list (* Append Int to accumulator and continue recursion *)
    | Int _ :: _ -> append_to_int_list lst [] ((StringList (List.rev acc)) :: list_list)(* Stop recursion and return StringList when a String is encountered *)
  in

  append_to_int_list l [] []


let test_in = [Int 2; Int 3; String "a"; String "b"; Int 4; String "c"]
let test_out = [IntList [2;3]; StringList ["a";"b"]; IntList [4]; StringList ["c"]]
let _ = assert (convert test_in = test_out)