(* Tanzim Mokammel *)
(* mtanzim@gmail.com *)

(* 
   NOTE: Main lesson based on solution set:
   - No need to use variables in recursion to keep track of "state"
   such as counters, accumulators, arrays
   - Example: See number_in_month_solution
*)

(* date: year, month, date *)
fun is_older (dateA:int*int*int, dateB:int*int*int) = 
   (#1 dateA) < (#1 dateB) 
   orelse 
      (#1 dateA) = (#1 dateB) andalso  (#2 dateA) < (#2 dateB) 
   orelse 
      (#1 dateA) = (#1 dateB) andalso  (#2 dateA) = (#2 dateB) andalso  (#3 dateA) < (#3 dateB)

fun oldest(date_list:(int*int*int) list) = 
   if null date_list
   then NONE
   else
      let val oldest_date = oldest(tl date_list)
      in
         if isSome oldest_date andalso is_older(valOf oldest_date, hd date_list)
         then oldest_date
         else SOME (hd date_list)
      end


fun number_in_month_solution (dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month) 

(* VS *)

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

fun get_nth(str_list:string list, pos: int) = 
   let fun count_pos(cur_list: string list, cur_pos: int ) = 
      if null cur_list
      then "BAD"
      else
         if cur_pos = pos -1
         then hd cur_list
         else count_pos(tl cur_list, cur_pos + 1)
   in
      count_pos(str_list, 0)
   end

fun date_to_string(date:int*int*int) =  
   let val month_lookup = [
      "January",
      "February",
      "March",
      "April",
      "May",
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December"
   ]
   in 
      get_nth(month_lookup, #2 date) ^ " " ^
         Int.toString (#3 date) ^ ", " ^
         Int.toString (#1 date)  
   end

(* all numbers are positive in in num_list *)
(* return position n *)
fun number_before_reaching_sum (sum: int, num_list: int list) =
   let fun sum_list(cur_list: int list, cur_sum: int, cur_pos: int) = 
      (* basic error checking to prevent overflow, 
      not required by assignment *)
      if null cur_list
      then ~1
      else
         if hd cur_list + cur_sum >= sum
         then cur_pos - 1
         else sum_list(tl cur_list, cur_sum + hd cur_list, cur_pos + 1)
   in
      sum_list(num_list, 0, 1)
   end


fun what_month(day: int) = 
   let val days_in_month = [
      31,
      28,
      31,
      30,
      31,
      30,
      31,
      31,
      30,
      31,
      30,
      31
   ]
   in
      number_before_reaching_sum(day, days_in_month) + 1
   end


fun month_range(range_tuple: int * int) = 
   let fun append_months(cur_day: int, cur_list: int list) =
      if cur_day > #2 range_tuple
      (* rev is allowed? *)
      then rev cur_list
      else append_months(cur_day + 1, what_month(cur_day):: cur_list )
   in 
      append_months(#1 range_tuple, [])
   end


