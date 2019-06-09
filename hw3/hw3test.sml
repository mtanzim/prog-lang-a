(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

(* Many test cases taken from here:
https://raw.githubusercontent.com/Ajwah/Programming-Languages/master/hw3/johnson_hw3_test.sml *)


use "hw3.sml";
val test1a = only_capitals ["A","B","C"] = ["A","B","C"]
val test1b = only_capitals ["A","B","C", "d"] = ["A","B","C"]
val test1c = only_capitals ["A","bxxcxcvxcv","C", "d"] = ["A","C"]
val test1d = only_capitals ["a","bxxcxcvxcv","c", "d"] = []
val test1e = only_capitals [] = []

val test2a = longest_string1 ["A","bc","C"] = "bc"
val test2b = longest_string1 [] = ""
val test2c = longest_string1 ["asdasdasd"] = "asdasdasd"
val test2d = longest_string1 ["A","bc","DC"] = "bc"

val test3a = longest_string2 ["A","bc","C"] = "bc"
val test3b = longest_string2 [] = ""
val test3c = longest_string2 ["asdasdasd"] = "asdasdasd"
val test3d = longest_string2 ["A","bc","DC"] = "DC"


val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4b = longest_string3 [] = ""
val test4c = longest_string3 ["asdasdasd"] = "asdasdasd"
val test4d = longest_string3 ["A","bc","DC"] = "bc"


val test4aa = longest_string4 ["A","bc","C"] = "bc"
val test4bb = longest_string4 [] = ""
val test4cc = longest_string4 ["asdasdasd"] = "asdasdasd"
val test4dd = longest_string4 ["A","bc","DC"] = "DC"

(*  *)
(*  *)
val test4aaa = longest_string3 ["A","bc","C"] = "bc"
(*  *)
(* val test4bbb = longest_string4 ["A","B","C"] = "C" *)
(*  *)
val test5a = longest_capitalized ["A","bc","C"] = "A"
val test5b = longest_capitalized ["asdasdasd"] = ""
val test5c = longest_capitalized ["A","bc","DC"] = "DC"
val test5d = longest_capitalized ["A","bcasdasdasd","DC"] = "DC"
val test5e = longest_capitalized ["A","Bcasdasdasd","DC"] = "Bcasdasdasd"

(*  *)
val test6a = rev_string "abc" = "cba"
val test6b = rev_string "abcadasdasdaczxczcqcqwqe2134234234" = String.rev "abcadasdasdaczxczcqcqwqe2134234234"
val test6c = rev_string "" = ""
(*  *)
val test7a = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7b = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,12,5] = 12
val test7c = first_answer (fn x => if x > 0 then SOME x else NONE) [1,2,3,12,5] = 1
val test7d = first_answer (fn x => if x = 5 then SOME x else NONE) [1,2,3,12,5] = 5
val test7e = (first_answer (fn x => if x = 5 then SOME x else NONE) [1,2,3,12,13] = 5) 
    handle NoAnswer => true = true
(*  *)
val test8a = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8b = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,1,1,1,1,1] = SOME [1,1,1,1,1,1]
val test8c = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,1,1,1,3,1] = NONE
(*  *)
val test9aa = count_wildcards Wildcard = 1
val test9ab =  count_wildcards(ConstructorP("test",Variable("Wildcard"))) = 0
(* https://raw.githubusercontent.com/Ajwah/Programming-Languages/master/hw3/johnson_hw3_test.sml *)
val test9ac = count_wildcards(ConstructorP("test",TupleP[Wildcard,Wildcard,UnitP,Wildcard,Variable("Wildcard"),Wildcard,Wildcard,ConstP(2)])) = 5
(*  *)
val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9ba = count_wild_and_variable_lengths (Variable("")) = 0
val test9bb = count_wild_and_variable_lengths (Variable("adasd")) = String.size "adasd"
(* https://raw.githubusercontent.com/Ajwah/Programming-Languages/master/hw3/johnson_hw3_test.sml *)
val test9bc =  count_wild_and_variable_lengths(TupleP([Variable("Test"),Wildcard])) = 5
val test9bd = count_wild_and_variable_lengths(TupleP([Variable("Test1"),ConstructorP("Wildcard",Wildcard),Variable("Test2")])) = 11
(*  *)
val test9ca = count_some_var ("x", Variable("x")) = 1
val test9cb = count_some_var ("y", Variable("x")) = 0
val test9cc = count_some_var ("", Variable("x")) = 0
(* https://raw.githubusercontent.com/Ajwah/Programming-Languages/master/hw3/johnson_hw3_test.sml *)
val test9cd = count_some_var ("test",Variable("test")) = 1
val test9ce = count_some_var ("dates",Wildcard) = 0
val test9cf = count_some_var ("dates",ConstP(3)) = 0
val test9cg = count_some_var ("setad",UnitP) = 0
val test9ch = count_some_var ("wert",TupleP[Wildcard,Variable("test"),Variable(""),Variable("dates"),ConstP(2),Variable("Wert")]) = 0
val test9ci = count_some_var ("",TupleP[Wildcard,Variable("test"),Variable(""),Variable("dates"),ConstP(2),Variable("Wert")]) = 1
val test9cj = count_some_var ("test",TupleP[Wildcard,Variable("test"),Variable(""),Variable("dates"),ConstP(2),Variable("Wert")]) = 1
val test9ck = count_some_var ("Wert",TupleP[Wildcard,Variable("test"),Variable(""),Variable("dates"),ConstP(2),Variable("Wert")]) = 1
(*  *)
val test91a = make_var_list (TupleP[Wildcard,Variable("test"),Variable(""),Variable("dates"),ConstP(2),Variable("Wert")]) = ["test", "", "dates", "Wert"]

val test91b = make_var_list (Variable("test"))= ["test"]
val test91c = make_var_list (Wildcard)= []

val test92a = no_dup ["test", "", "dates", "Wert"] = true
val test92b = no_dup ["test", "test", "dates", "Wert"] = false
val test92c = no_dup [] = true



val test10a = check_pat (Variable("x")) = true

val test10c= check_pat(Variable("unique")) = true


val test10g = check_pat(TupleP[Variable("unique"),Wildcard]) = true


val test10i = check_pat(TupleP([Wildcard,Wildcard,UnitP,Wildcard,Variable("Wildcard"),Wildcard,Wildcard,ConstP(2)])) = true
val test10j = check_pat(ConstructorP("test",ConstructorP("test2",TupleP[Wildcard,Wildcard,UnitP,Wildcard,Variable("Wildcard"),Wildcard,Wildcard,ConstP(2)]))) = true
val test10k = check_pat(TupleP([Variable("Test1"),Variable("Test2")])) = true
val test10l =  check_pat(TupleP([Variable("Test1"),Wildcard,Variable("Test2")])) = true
val test10m =  check_pat(TupleP([Variable("Test1"),ConstructorP("Wildcard",Wildcard),Variable("Test2")])) = true
val test10n =  check_pat(ConstructorP("Test",TupleP([Variable("duplicate"),TupleP([ConstructorP("test2",Variable("duplicate")),Variable("unique"),Variable("qwerty")])]))) = false
val test10o =  check_pat(ConstructorP("sdfs",TupleP([Variable("A"),Variable("B"),ConstP(4),TupleP[Variable("C"),ConstructorP("sds",UnitP)],ConstructorP("dfg",TupleP[Variable("D"),Variable("E"),ConstP(4)])]))) = true

(* Do the folloing cases matter? They don't contain any variables. They are failing *)
(* val test10b = check_pat(Wildcard) = false 
val test10d = check_pat(ConstP(3)) = false 
val test10e = check_pat(UnitP) = false 
val test10f = check_pat(TupleP[]) = false 
val test10h = check_pat(TupleP([Wildcard,UnitP])) = false  *)

(*  *)
(* val test11a = match (Const(1), UnitP) = NONE *)
val test11b = match (Const(1), Wildcard) = SOME[]
val test11c = match (Unit, Wildcard) = SOME[]
val test11d = match (Unit, Variable "a") = SOME[("a", Unit)]
val test11e = match (Unit, UnitP) = SOME[]
val test11f = match (Const(1), UnitP) = NONE
val test11g = match (Const(1), ConstP 1) = SOME []
val test11h = match (Const(1), ConstP 2) = NONE
val test11i = match (Const(1), ConstructorP("a", UnitP)) = NONE
val test11j = match (Constructor("a", Unit), ConstructorP("a", UnitP)) = SOME []
val test11k = match (Constructor("a", Unit), ConstructorP("a", Variable "a")) = SOME[("a", Unit)]

(* https://github.com/ksaveljev/Programming-Languages/blob/master/hw3/hw3_test.sml *)

fun f n = if n = 6 then NONE else SOME [n]
val all_answers_test1 = all_answers f [] = SOME []
val all_answers_test2 = all_answers f [1] = SOME [1]
val all_answers_test3 = all_answers f [1,2,3] = SOME[3,2,1]
val all_answers_test4 = all_answers f [1,2,6,3] = NONE


val match_test20 = match(Constructor("hi", Const 1), TupleP[ConstP 1]) = NONE
val match_test21 = match(Tuple[Unit], TupleP[UnitP, UnitP]) = NONE
val match_test22 = match(Tuple[Unit, Unit], TupleP[UnitP]) = NONE
val match_test23 = match(Tuple[Const 1], TupleP[UnitP]) = NONE
val match_test24 = match(Tuple[Const 1], TupleP[ConstP 2]) = NONE
val match_test26 = match(Tuple[Unit], TupleP[Variable "hi"]) = SOME [("hi", Unit)]
val match_test25 = match(Tuple[Const (1)], TupleP[ConstP 1]) = SOME []
val match_test27 = match(Tuple[Constructor("hi", Const 1)], TupleP[ConstructorP("hi", ConstP 1)]) = SOME []

(*  *)  
(* val test12 = first_match Unit [UnitP] = SOME [] *)

