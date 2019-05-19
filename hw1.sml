(* year, month, date *)
fun is_older (dateA:int*int*int, dateB:int*int*int) = 
    (#1 dateA) > (#1 dateB)
