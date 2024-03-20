(* Deepest Parentheses

   Implement the function `deepest_parens` which, given

     ps : paren list with a balanced set of parentheses

   returns the index of (the left parentheses of) the leftmost deepest
   pair of parentheses in `ps`.  You should use List.fold_left.

   Examples:
   let _ = assert (deepest_parens [Op;Cl] = 0)                       ()
   let _ = assert (deepest_parens [Op;Cl;Op;Op;Cl;Cl] = 3)           ()(())
   let _ = assert (deepest_parens [Op;Op;Cl;Op;Cl;Cl] = 1)           (()())
   let _ = assert (deepest_parens [Op;Op;Cl;Op;Op;Cl;Cl;Cl] = 4)     (()(()))

   Note to self/TFs/TAs: There are a number of ways to do this, using
   records is one way.

*)

type paren
  = Op
  | Cl

let deepest_parens (ps : paren list) : int =
  assert false (* TODO *)

ORDER OF IMPORTANCE
  1. not
  2. and
  3. or

<expr> ::= <expr> or <expr2> | <expr2>      (*changing (expr2) to <expr> allows us to get left associativity yuh yerr wuz good yo*)
<expr2> ::= <expr2> and <expr3> | <expr3>
<expr3> ::= not <expr4> | <expr4>
<expr4> ::= <bool> | (<expr>)



  <expr>
  <term>
  <pars> * <var>
  (<expr>) * <var>
  (<expr> + <term>) * <var>
  (<expr> + <pars>) * <var>
  (<expr> + <var>) * <var>
  (<term> + <var>) * <var>
  (<pars> + <var>) * <var>
  (<var> + <var>) * <var>
  (x + <var>) * <var>
  (x + y) * <var>
  (x + y) * z


  type expr
    = C1 of expr2
    = C2 of expr2 * expr
  and expr2 
    = x1
    | Paren of expr

type expr
  = x
  | Add of expr * expr