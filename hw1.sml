(* year, month, date *)
fun is_older_old (dateA:int*int*int, dateB:int*int*int) = 
   if (#1 dateA) < (#1 dateB)
   then true
   else
      if (#2 dateA) < (#2 dateB)
      then true
      else 
         (#3 dateA) < (#3 dateB)

fun is_older (dateA:int*int*int, dateB:int*int*int) = 
   (#1 dateA) < (#1 dateB) 
      orelse (#2 dateA) < (#2 dateB) 
      orelse (#3 dateA) < (#3 dateB)

fun number_in_month(date_list:(int*int*int) list, month) = 
   let fun count(cur_val: int, cur_list: (int*int*int) list) =
      if null cur_list
      then cur_val
      else 
         if #2 (hd cur_list) = month
         then count(cur_val + 1,tl cur_list)
         else count(cur_val, tl cur_list)
   in
      count(0, date_list)
   end

      

         

