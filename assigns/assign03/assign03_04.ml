(* Matrices

   In this problem you will be building a (very) small interface for
   matrices.  A matrix is represented as a record which keeps track of
   its numbers of rows and columns as well as the values represented
   as a list of rows (i.e., a list of lists).  You will be
   implementing some error handling by working with `result`s.

   ************

   Task 1: Implement a function `mkMatrix` which given

     rs : a list of lists

   returns a matrix represented by this list of lists if it is valid
   or an `error` otherwise.  The error conditions are:

   * If the lengths of the rows in `rs` are not all equal then
   `mkMatrix` should return `Error UnevenRows`.

   * If `rs` is empty then `mkMatrix` should return `Error ZeroRows`.

   * If `rs` contains only empty lists, then `mkMatrix` should reutrn
   `Error ZeroCols`.

   In other words, `mkMatrix` should only return a matrix if `rs`
   represents a nonempty rectangular grid of values.

   Example:
   let l = [[1;2;3];[4;5;6]]
   let rm = mkMatrix l
   let _ = match rm with
     | Ok m ->
       let _ = assert (m.num_rows = 2) in
       let _ = assert (m.num_cols = 3) in
       ()
     | _ -> assert false

   let r = [[1;2;3;4];[1;2;3]]
   let rm' = mkMatrix r
   let _ = match rm' with
     | Ok _ -> assert false
     | Error e -> assert (e = UnevenRows)

   ************

   Task 2: Implement the function `transpose` which, given

     m : a matrix

   returns the transpose of `m`.

   Example:
   let _ = match rm with
     | Ok m ->
       let tm = transpose m in
       let _ = assert (tm.num_rows = 3) in
       let _ = assert (tm.num_cols = 2) in
       let _ = assert (tm.rows = [[1;4];[2;5];[3;6]]) in
       ()
     | _ -> assert false

   ************

   Task 3: Implement the function `multiply` which, given

     m : a matrix
     n : a matrix

   returns the product of `m` and `n` if it is possible to multiply
   them. The error condition:

   * If the number of columns of `m` is not the same as the number of
   rows of `n`, then `multiply` should return `Error MulMismatch`

   Example:
   let a =
     { num_rows = 2 ;
       num_cols = 2 ;
       rows = [[1.;2.];[3.;4.]] ;
     }

   let b =
     { numn_rows = 1 ;
       num_cols = 2 ;
       rows = [[1.; 2.]] ;
     }

   let _ = assert (multiply a a = Ok {
     num_rows = 2 ;
     num_cols = 2 ;
     rows = [[7.;10.];[15.;22.]] ;
    })

   let _ = assert (multiply a b = Error MulMismatch)

   ************

   References:
   https://en.wikipedia.org/wiki/Matrix_multiplication
   https://en.wikipedia.org/wiki/Transpose
   https://www.cs.bu.edu/fac/crovella/cs132-book/landing-page.html

*)

type error
   = UnevenRows
   | ZeroRows
   | ZeroCols
   | MulMismatch

type 'a matrix = {
  num_rows : int ;
  num_cols : int ;
  rows : ('a list) list ;
}




let rec len (series : 'a list) (acc : int): int = 
  match series with
  | [] -> acc
  | h::t -> len t (acc+1)

let append (mat : 'a matrix) (series : 'a list) =
  {mat with 
  rows = series :: mat.rows; 
  num_cols = mat.num_cols + 1;
  num_rows = (len series 0)}


let mkMatrix (rs : 'a list list) : ('a matrix, error) result =
  let rec helper r m n acc =
    match r with
    | [] ->
      if m > 0 && n > 0 then Ok acc
      else if n = 0 then Error ZeroRows
      else Error ZeroCols
    | h :: t ->
      let x = List.length h in
      match m, n with
      | -1, -1 -> helper t 1 x {num_rows = m; num_cols = x; rows = [h]}
      | _, _ ->
        if x <> n then Error UnevenRows
        else helper t (m + 1) n {acc with rows = acc.rows @ [h]}
  in
  match rs with
  | [] -> Error ZeroRows
  | h :: _ ->
    let a = {num_rows = 0; num_cols = 0; rows = [[]]} in
    helper rs (-1) (-1) a


let transpose (m : 'a matrix) : 'a matrix =
  (* check that everything is copasetic *)
  let num_rows = m.num_rows in
  let num_cols = m.num_cols in
  let transpose_mat matrix =
    match matrix with
    | [] -> []
    | []::_ -> []
    | _ ->
      let rec aux acc = function
        | []::_ -> List.rev acc
        | m -> aux ((List.map List.hd m)::acc) (List.map List.tl m)
      in aux [] matrix
  in
  { num_rows = num_cols;
    num_cols = num_rows;
    rows = transpose_mat m.rows;
  }

let multiply (m : float matrix) (n : float matrix) : (float matrix, error) result =
  let rec dot_product row col =
    match row, col with
    | [], [] -> 0.0
    | x::xs, y::ys -> x *. y +. dot_product xs ys
    | _ -> failwith "Uneven row/column lengths in matrix multiplication"
  in
  let rec multiply_row_with_cols row cols =
    match cols with
    | [] -> []
    | col :: rest ->
      let product = dot_product row col in
      product :: multiply_row_with_cols row rest
  in
  let rec multiply_rows_with_cols rows cols =
    match rows with
    | [] -> []
    | row :: rest ->
      let new_row = multiply_row_with_cols row cols in
      new_row :: multiply_rows_with_cols rest cols
  in
  if m.num_cols <> n.num_rows then
    Error MulMismatch
  else
    let transposed_n = transpose n in
    let multiplied_rows = multiply_rows_with_cols m.rows transposed_n.rows in
    Ok { num_rows = m.num_rows; num_cols = n.num_cols; rows = multiplied_rows }


let a2 =
  { num_rows = 2 ;
    num_cols = 2 ;
    rows = [[1.;2.;3.];[4.;5.;6.]] ;
  }

let a =
  { num_rows = 2 ;
    num_cols = 2 ;
    rows = [[1.;2.];[3.;4.]] ;
  }

let _ = assert (multiply a a = Ok {
  num_rows = 2 ;
  num_cols = 2 ;
  rows = [[7.;10.];[15.;22.]] ;
  })