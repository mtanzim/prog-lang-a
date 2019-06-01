(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1a = all_except_option ("string", []) = NONE
val test1b = all_except_option ("", []) = NONE
val test1c = all_except_option ("string", ["string"]) = SOME []
val test1d = all_except_option ("string", ["string","asdsd"]) = SOME ["asdsd"]
val test1e = all_except_option ("string", ["string","asdsd","asdasd"]) = SOME ["asdsd","asdasd"]
val test1f = all_except_option ("stringa", ["string","asdsd","asdasd"]) = NONE

val test2a = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2b = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = 
                                    ["Fredrick","Freddie","F"] 
val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test4b = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]
val test4c = similar_names ([], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}]

val test5a = card_color (Clubs, Num 2) = Black
val test5b = card_color (Spades, Num 3) = Black
val test5c = card_color (Diamonds, Num 233) = Red
val test5d = card_color (Hearts, Num 212) = Red

val test6a = card_value (Clubs, Num 2) = 2
val test6b = card_value (Clubs, Num 3) = 3
val test6c = card_value (Clubs, Num 4) = 4
val test6d = card_value (Clubs, Num 5) = 5
val test6e = card_value (Clubs, Ace) = 10
val test6f = card_value (Clubs, King) = 10

val test7a = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test7b = remove_card ([(Hearts, Ace), (Hearts, Queen)], (Hearts, Ace), IllegalMove) = [(Hearts, Queen)]
val test7c = remove_card ([(Hearts, Ace), (Hearts, Queen), (Hearts, Num 2)], (Hearts, Ace), IllegalMove) = [(Hearts, Queen), (Hearts, Num 2)]

(* Only removing the first one works *)
val test7d = remove_card ([(Hearts, Ace), (Hearts, Queen), (Hearts, Num 2)], (Hearts, Queen), IllegalMove) = [(Hearts, Ace), (Hearts, Num 2)]
val test7e = remove_card ([(Hearts, Ace), (Hearts, Queen), (Hearts, Num 2)], (Hearts, Num 2), IllegalMove) = [(Hearts, Ace), (Hearts, Queen)]
val test7f = remove_card ([], (Hearts, Num 2), IllegalMove) = []
(* val test7f = remove_card ([(Hearts, Ace), (Hearts, Queen), (Hearts, Num 2)], (Hearts, Num 4), IllegalMove) handle IllegalMove => true = true *)

(* val test7b = remove_card ([(Hearts, Ace), (Hearts, Queen), (Hearts, Num 2)], (Hearts, Queen), IllegalMove) = [(Hearts, Ace), (Hearts, Num 2)] *)
(* val test7b = remove_card ([(Hearts, Queen)], (Hearts, Ace), IllegalMove) = IllegalMove *)

val test8a = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test8b = all_same_color [(Clubs, Ace), (Hearts, Ace)] = false
val test8c = all_same_color [(Clubs, Ace), (Spades, Ace)] = true
val test8d = all_same_color [(Diamonds, Ace), (Hearts, Ace)] = true
val test8e = all_same_color [(Diamonds, Ace)] = true
val test8f = all_same_color [] = true
val test8g = all_same_color [(Diamonds, Ace), (Hearts, Ace), (Spades, Ace)] = false
val test8h = all_same_color [(Spades, Ace), (Hearts, Ace), (Diamonds, Ace)] = false
val test8i = all_same_color [(Diamonds, Ace), (Spades, Ace), (Hearts, Ace)] = false
val test8k = all_same_color [(Diamonds, Ace), (Diamonds, Ace), (Hearts, Ace), (Hearts, Ace)] = true
val test8l = all_same_color [(Diamonds, Ace), (Diamonds, Ace), (Hearts, Ace), (Spades, Ace)] = false

(* val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4 *)

(* val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4 *)

(* val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6 *)

(* val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3 *)

(* val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true) *)
             
             
