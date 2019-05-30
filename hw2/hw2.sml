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
        case all_except_option(str_val, str_list) of
            NONE => []
            | SOME lst => lst
    in 
        case str_lists of 
            [] => []
            | head::rest => each_list(head) @ get_substitutions1(rest, str_val)
    end

(* cheat for now *)
fun get_substitutions2(str_lists, str_val) = 
    let fun each_list (str_list) = 
        case all_except_option(str_val, str_list) of
            NONE => []
            | SOME lst => lst
    in 
        case str_lists of 
            [] => []
            | head::rest => each_list(head) @ get_substitutions1(rest, str_val)
    end
    

fun similar_names(str_lists, r : {first:string,middle:string,last:string}) =
    let val {first=x,middle=y,last=z} = r
    in
        let fun make_list(cur_list) =
            case cur_list of 
                [] => []
                | head::rest => {first=head, last=z, middle=y}::make_list(rest)
        in 
            case get_substitutions1(str_lists, x) of
                [] => [r]
                | lst => r::make_list(lst)
        end
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
fun card_color (suit_in, rank_in) =
    case suit_in of 
        Clubs => Black
        | Spades => Black
        | _ => Red

fun card_value (suit_in, rank_in) =
    case rank_in of 
        Num i => i
        | _ => 10

fun remove_card(cs,c, e) = 
    let fun also_track_isfound (cur_cs, is_found) = 
        case (cs,is_found) of
            ([], true) => []
            | ([], false) => raise e
            | (head::rest, false) => 
                if head = c
                then also_track_isfound(rest, true)
                else head::also_track_isfound(rest,false)
            | (head::rest, true) => rest
    in 
        also_track_isfound(cs, false)
    end
    (* case cs of
        [] => []
        | head::rest => 
            if head = c
            then remove_card(rest,c, e)
            else head::remove_card(rest,c, e) *)