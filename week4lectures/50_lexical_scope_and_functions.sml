(* Programming Languages, Dan Grossman *)
(* Section 3: Lexical Scope and Higher-Order Functions *)

(* first example *)
val x = 1
(* creates a closure! -> function body, plus it's current, frozen environment *)
fun f y = 
    let 
        val x = y+1
    in
        fn z => x + y  + z
    end
val x = 3 (* irrelevant *)
val g = f 4 
val y = 5 (* irrelevant *)
val z = g 6

(* second example *)
fun f g = 
    let 
        val x = 3
    in
        g 2
    end
val x = 4
fun h y = x + y (* always adds 4 to its args due to its closure *)
val z = f h
