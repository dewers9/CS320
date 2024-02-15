(* Boolean Expressions

   Implement a function `eval` which given

     v: an association list mapping `string`s to `bool`s
     e: a boolean expression (see below)

   returns `Some b` if every variable in `e` also appears in `v` and
   `e` evaluates to `b`, or `None` if not all variable in `e` appear
   in `v`.

   Example:
   let v = [("a", true); ("b", false); ("c", true)]
   let e = And (Var "a", Or (Var "b", Var "c"))
   let f = Not (Var "d")
   let _ = assert (eval v e = Some true)
   let _ = assert (eval v f = None)

   One way to think about evaluation: imagine `v` as defining a
   collection of `bools` in OCaml:

     let a = true
     let b = false
     let c = true

   and an expression as defining a boolean expression in OCaml:

     let e = a && (b || c)

   The goal of evaluation is to determine the value of `e`.

   Likewise, if you define an expression with a name that has not
   been defined, you would get a compile-time error

     let f = not d

   which is why the function `eval` should return `None` on `f`.

   Hint: Take a look at the textbook section on association lists
   (they are a simple implementation of a dictionary-like data
   structure), as well as the function List.assoc_opt.

*)

type bexp =
  | Var of string
  | Not of bexp
  | And of bexp * bexp
  | Or of bexp * bexp

let rec eval (v : (string * bool) list) (e : bexp) : bool option =
  
  let rec get_bool (v_in: (string * bool) list) (id:string) : bool option =
    match v_in with
    | [] -> None
    | h::t ->
      (match h with
      |(this_id,tf) -> 
          if this_id = id then Some tf
        else
          get_bool t id)
  in
  
  match e with
  | And (exp1, exp2) ->
    (match (eval v exp1, eval v exp2) with
    | (Some true, Some true) -> Some true
    | (None, _) | (_, None) -> None
    | _ -> Some false)
  | Or (exp1, exp2) ->
    (match (eval v exp1, eval v exp2) with
    | (Some true, _) | (_, Some true) -> Some true
    | (None, _) | (_, None) -> None
    | _ -> Some false)
  | Not exp ->
    (match (eval v exp) with
    | None -> None
    | Some true -> Some false
    | Some false -> Some true
    | _ -> None)
  | Var exp ->
    get_bool v exp
  


let v = [("a", true); ("b", true); ("c", true)]
let e = And (Var "a", Or (Var "b", Var "c"))
let f = Not (Var "d")
let _ = assert (eval v e = Some true)
let _ = assert (eval v f = None)