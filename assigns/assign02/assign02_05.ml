(* Listing Paths

   Implement a function `all_paths` which given

     len : a nonnegative integer
     stp : a point (see below)
     endp : a point

   returns a list of `(dir * int)` lists, where each list contains
   sequence of directions to get from `stp` to `endp` in exactly `len`
   steps.

   Notes:
   A sequence of directions is a list of `dir * int` pairs with the
   property that no adjacent elements have the same `dir`.  For example,

     [(N, 1); (S, 1); (N, 3)]

   is a valid sequence of directions but

     [(N, 1); (N, 3); (S, 1)]

   is not. Instead, this should be

     [(N, 4), (S, 1)]

   Examples:
   let origin = {x=0;y=0}
   let _ = assert (all_paths 0 origin origin = [[]])
   let _ = assert (all_paths 1 origin origin = [])
   let _ = assert (all_paths 2 origin origin =
       [[(N,1);(S,1)] ;
        [(S,1);(N,1)] ;
        [(E,1);(W,1)] ;
        [(W,1);(E,1)] ])
   let _ = assert (all_paths 3 origin origin = [])
   let _ = assert (List.mem [(N,2);(S,2)] (all_paths 4 origin origin))
   let _ = assert (List.mem [(N,1);(E,1);(W,1);(S,1)] (all_paths 4 origin origin))

*)

type dir = N | S | E | W

type point = {
  x : int ;
  y : int ;
}

let origin = { x = 0; y = 0 }

let inc_x p =
  { p with x = p.x + 1 }
let dec_x p =
  { p with x = p.x - 1 }
let inc_y p =
  { p with y = p.y + 1 }
let dec_y p =
  { p with y = p.y - 1 }

let drop_empty_lists lists =
  List.filter (fun lst -> lst <> []) lists

let merge_adjacent_same_directions sublist =
  let rec merge acc = function
    | [] -> List.rev acc
    | (direction1, num1)::(direction2, num2)::rest when direction1 = direction2 ->
        merge acc ((direction1, num1 + num2)::rest)
    | tuple::rest ->
        merge (tuple :: acc) rest
  in
  merge [] sublist

let merge_adjacent_same_directions_in_lists list_of_lists =
  List.map merge_adjacent_same_directions list_of_lists

let rec gen_paths (rem_len) (curr_stp) (endp) (prev_steps : (dir * int) list) : (dir * int) list list  =
  if rem_len = 0 then
    if curr_stp.x = endp.x && curr_stp.y = endp.y then
      [List.rev prev_steps]
    else
      [[]]
  else
    gen_paths (rem_len - 1) (inc_y curr_stp) (endp) ((N,1) :: prev_steps) @
    gen_paths (rem_len - 1) (dec_y curr_stp) (endp) ((S,1) :: prev_steps) @
    gen_paths (rem_len - 1) (inc_x curr_stp) (endp) ((E,1) :: prev_steps) @
    gen_paths (rem_len - 1) (dec_x curr_stp) (endp) ((W,1) :: prev_steps)

(* let rec check_valid (prev_steps) (curr_len) (curr_stp) (endp : point) (acc)=
  if curr_len == 0 *)


let rec all_paths (len : int) (stp : point) (endp : point) : (dir * int) list list =

  merge_adjacent_same_directions_in_lists (drop_empty_lists (gen_paths len stp endp []))