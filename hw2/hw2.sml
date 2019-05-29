(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* 1. This problem involves using first-name substitutions to come up with alternate names. For example,
Fredrick William Smith could also be Fred William Smith or Freddie William Smith. Only part (d) is
specifically about this, but the other problems are helpful.
(a) Write a function all_except_option, which takes a string and a string list. Return NONE if the
string is not in the list, else return SOME lst where lst is identical to the argument list except the string
is not in it. You may assume the string is in the list at most once. Use same_string, provided to you,
to compare strings. Sample solution is around 8 lines. *)


fun all_except_option (str_val, xs) =
    case xs of
        [] => NONE
        | first::rest => 
            if same_string(str_val, first)
            (* note: word occurs only once *)
            then SOME rest
            else case all_except_option(str_val, rest) of
                NONE => NONE
                | SOME tail => SOME (first::tail)

fun get_substitutions1(str_lists, str_val) = 
    let fun each_list (str_list) = 
        case str_list of
            [] => []
            | head::rest => 
                if same_string(head,str_val)
                then each_list(rest)
                else
                    let fun each_str(cur_str) =
                        case cur_str of
                            head_str ^ rest_str => same_string(head_str ^ rest_str, str_val) orelse each_str(rest_str)
                            | _ => false
                    in
                        case each_str (head) of 
                            _  => head::each_list(rest)
                    end
    in 
        case str_lists of 
            [] => []
            | head::rest => each_list(head) @ get_substitutions1(rest, str_val)
    end

        


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
