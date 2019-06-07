(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

fun count_wildcards p = 
	g (fn () => 1) (fn _ => 0) p
	
fun count_wild_and_variable_lengths p = 
	g (fn () => 1) (String.size) p
	(* let val wild = g (fn () => 1)
	in
		wild (fn _ => 0) p + wild (String.size) p
	end *)

fun count_some_var (s,p) =
	g (fn () => 0) (fn x => if (x = s) then 1 else 0 ) p

fun only_capitals xs =
	List.filter (fn x => Char.isUpper (String.sub(x,0)) ) xs

fun longest_string1 xs = 
	List.foldl (fn (x, y) => case (String.size x > String.size y ) of true => x | _ => y ) "" xs
fun longest_string2 xs = 
	List.foldl (fn (x, y) => case (String.size x >= String.size y ) of true => x | _ => y ) "" xs

fun longest_string_helper f xs = 
	List.foldl (fn (x, y) => case f(String.size x, String.size y) of true => x | _ => y ) "" xs

fun longest_string3 xs = longest_string_helper (fn (a, b) => a > b) xs
fun longest_string4 xs = longest_string_helper (fn (a, b) => a >= b) xs

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs =
	case xs of 
		[] => raise NoAnswer
		| head::rest => case (f head) of 
			SOME y => y
			| NONE => first_answer f rest

fun all_answers f xs = 
	let fun inner (cur_xs, acc) =
		case cur_xs of 
			[] => SOME acc
			| head::rest => case (f head) of 
				SOME [y] => inner(rest, [y] @ acc)
				| NONE => NONE
	in
		inner(xs, [])
	end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
