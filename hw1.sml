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

fun number_in_month(date_list:(int*int*int) list, month: int) = 
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

fun number_in_months(date_list:(int*int*int) list, month_list: int list) = 
   let fun count_month(cur_val: int, cur_month_list: int list) =
      if null cur_month_list
      then cur_val
      else
         count_month(cur_val + number_in_month(date_list, hd cur_month_list),
          tl cur_month_list)
   in
      count_month(0,month_list)
   end
   

fun dates_in_month(date_list:(int*int*int) list, month: int) = 
   let fun append(cur_valid_list: (int*int*int) list, cur_list: (int*int*int) list) =
      if null cur_list
      (* rev may not be allowed *)
      then rev cur_valid_list
      else 
         if #2 (hd cur_list) = month
         then append(hd cur_list::cur_valid_list,tl cur_list)
         else append(cur_valid_list, tl cur_list)
   in
      append([], date_list)
   end

      

fun dates_in_months(date_list:(int*int*int) list, month_list: int list) = 
   let fun append_month(cur_val_list: (int*int*int) list, cur_month_list: int list) =
      if null cur_month_list
      then cur_val_list
      else
         append_month(cur_val_list @ dates_in_month(date_list, hd cur_month_list),
          tl cur_month_list)
   in
      append_month([],month_list)
   end

