let rec helper s row = 
    if String.length s <= row then
        String.cat (String.sub s 0 (String.length s)) (String.make (row - (String.length s)) '*' )
    else
        String.cat (String.cat (String.sub s 0 row) "\n") (helper (String.sub s (row - 1) ((String.length s) - row)) (row + 1))

let string_tri s = 
    helper s 1

let py_trip_hyp (n : int) =
    let a = n/2 in 
    let sqrt_int (a : int) : int = sqrt a in
