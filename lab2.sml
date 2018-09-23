
 

(*******************************************************************************************)
(*******************************************************************************************)
(* Question 3: Define a function iota so that iota n returns the list [0, 1, 2, 3, ..., n−1]. For example,
iota 5 should return [0, 1, 2, 3, 4].
	*)
(* Question 3.1: int −> int *)
(* fun1 x
   TYPE: int -> 
   PRE: only integer
   POST: x+1
   SIDE-EFFECTS: none
   EXAMPLES: fun1 1 = 2, fun1 3 = 4
*)

fun iota 0 = [] | iota n = iota(n-1) @ [n-1];

