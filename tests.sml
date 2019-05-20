(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


val test1a = is_older ((1,2,3),(2,3,4)) = true
val test1b = is_older ((2019,12,18),(2018,12,18)) = false
val test1c = is_older ((2019,12,18),(2019,11,18)) = false
val test1d = is_older ((2019,5,18),(2019,11,18)) = true
val test1e = is_older ((2019,11,11),(2019,11,18)) = true
val test1f = is_older ((2019,11,21),(2019,11,18)) = false
val test1g = is_older ((2019,11,21),(2019,11,21)) = false

val test2a = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test2b = number_in_month ([(2012,12,28),(2013,12,1)],12) = 2
val test2c = number_in_month ([],12) = 0
val test2d = number_in_month ([(2012,12,28),(2013,12,1)],125) = 0
val test2e = number_in_month ([(2012,12,28),(2013,12,1),(2012,12,28),(2013,12,1)],12) = 4
val test2f = number_in_month ([(2012,12,28),(2013,11,1),(2012,12,28),(2013,12,1)],12) = 3

val test3a = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3b = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,5]) = 3
val test3c = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28), (2011,5,28)],[2,3,4,5]) = 4
val test3d = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28), (2011,5,28)],[]) = 0
val test3e = number_in_months ([],[3,4]) = 0
val test3f = number_in_months ([],[]) = 0
val test3g = number_in_months ([(2012,1,28),(2013,1,1),(2011,1,31),(2011,1,28), (2011,1,28)],[1]) = 5

val test4a = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4b = dates_in_month ([(2012,2,28),(2013,12,1)],0) = []
val test4c = dates_in_month ([],0) = []
val test4d = dates_in_month ([(2012,2,28),(2013,12,1)],6) = []
val test4e = dates_in_month ([(2012,2,28),(2013,2,1)],2) = [(2012,2,28),(2013,2,1)]
val test4f = dates_in_month ([(2012,2,28),(2013,2,1), (2012,2,28),(2013,2,1), (2012,2,28),(2013,2,1)],2) = [(2012,2,28),(2013,2,1), (2012,2,28),(2013,2,1), (2012,2,28),(2013,2,1)]
val test4g = dates_in_month ([(2012,3,28),(2013,2,1), (2012,2,28),(2013,2,1), (2012,2,28),(2013,2,1)],2) = [(2013,2,1), (2012,2,28),(2013,2,1), (2012,2,28),(2013,2,1)]
(* val test4eWrong = dates_in_month ([(2012,2,28),(2013,2,1)],2) = [(2013,2,1), (2012,2,28)] *)

val test5a = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test5b = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,12,4]) = [(2012,2,28),(2013,12,1),(2011,4,28)]

(* val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31) *)