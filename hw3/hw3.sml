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




fun count_wildcards p = 
	g (fn () => 1) (fn _ => 0) p
	
fun count_wild_and_variable_lengths p = 
	g (fn () => 1) (String.size) p


fun no_dup xs = 
	case xs of 
		[] => true
		| head::rest => ((List.exists (fn x => x = head) rest) <> true) andalso no_dup rest

fun make_var_list p = 
	case p of 
		Variable x => x::[]	
		| TupleP ps => List.foldl (fn (cur_p,acc) => acc @ (make_var_list cur_p) ) [] ps
		| ConstructorP(_,p) => make_var_list p
		| _ => []

fun check_pat p = no_dup (make_var_list p)

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
				| SOME [] => inner(rest, [] @ acc)
				| NONE => NONE
	in
		inner(xs, [])
	end

fun match (v,p) = 
	case p of
		Wildcard => SOME []
		| Variable x => SOME [(x,v)]
		| UnitP =>
			(case v of 
				Unit => SOME[]
				| _ => NONE)
		| ConstP x => 
			(case v of 
				Const y => if x = y then SOME[] else NONE
				| _ => NONE)
		| ConstructorP (nameP, pat) => 
			(case v of
				Constructor (nameV, valll) => (if nameP = nameV then match(valll, pat) else NONE)
				| _ => NONE)
		| TupleP ps => 
			(case v of 
				Tuple vps => 
					if (List.length vps <> List.length ps) then NONE else
						all_answers match (ListPair.zip(vps, ps))
				| _ => NONE)

fun curry f x y = f (x,y)
fun first_match v ps =
	let val each_match = curry match v
	in
		 SOME [first_answer each_match ps]

	end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
