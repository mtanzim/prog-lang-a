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
    case str_lists of 
        [] => []
        | head::rest => 
            case all_except_option(str_val, head) of 
                NONE => get_substitutions1(rest, str_val)
                | SOME lst => lst @ get_substitutions1(rest, str_val)

(* tail recursive version *)
fun get_substitutions2(str_lists, str_val) = 
    let fun inner_sub (cur_list, cur_sub) =
        case cur_list of 
            [] => cur_sub
            | head::rest => 
                case all_except_option(str_val, head) of 
                    NONE => inner_sub(rest, cur_sub)
                    | SOME lst => inner_sub(rest, cur_sub @ lst)
    in
        inner_sub(str_lists, [])
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
fun get_card_from_discard move =
    case move of 
        Discard cur_card => cur_card



fun card_color (suit_in, rank_in) =
    case suit_in of 
        Clubs => Black
        | Spades => Black
        | _ => Red

fun card_value (suit_in, rank_in) =
    case rank_in of 
        Num i => i
        | Ace => 11
        | _ => 10

fun remove_card(cs,c, e) = 
    case cs of 
        [] => []
        | head::rest => 
            if head = c
            then rest
            else 
                case rest of [] => raise e
                | _ => head::remove_card(rest, c, e)
        (* | head::[] => 
            if head = c
            then []
            else raise e *)

fun all_same_color cs = 
    case cs of
        [] => true
        | head::[] => true
        | head::neck::rest =>  
                card_color(head) = card_color(neck) andalso
                all_same_color(neck::rest) 
        
fun sum_cards(cs) =
    let fun sum_inner(cur_cs, cur_sum) = 
        case cur_cs of
            [] => cur_sum
            | head::rest =>
                sum_inner(rest, card_value(head) + cur_sum)
        in
            sum_inner(cs, 0)
        end


fun score (cs,goal) = 
    let val sum = sum_cards(cs) 
    in
        case ( cs, sum > goal, all_same_color(cs) ) of
            ([],_,_) => goal
            | (_ ,true, false) => 3 * (sum - goal)
            | (_ ,false, false) => goal - sum
            | (_, true, true) => (3 * (sum - goal)) div 2
            | (_, false, true) => (goal - sum) div 2
    end


fun officiate (cs, ms, goal) = 
    let fun track_state (card_pile, cur_ms, held_cards) = 
        case (card_pile, cur_ms, sum_cards(held_cards) > goal) of
            ([],_,_) => score(held_cards,goal)
            | (_,[],_) => score(held_cards,goal)
            | (_,_,true) => score(held_cards,goal)
            | _ =>
                case cur_ms of
                    head_move::rest_moves => 
                        case head_move of 
                            Draw => let val (drawn_card::rest_pile) = card_pile
                                in
                                    track_state(rest_pile, rest_moves,drawn_card::held_cards)
                                end
                           | Discard discarded_card => 
                                track_state(card_pile, rest_moves, remove_card(held_cards, discarded_card, IllegalMove))
    in
        track_state(cs, ms, [])
    end