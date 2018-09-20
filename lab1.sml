
 

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


(* Intersection *) 
(* 2.1 inter s1 s2  *)

(* fun mergesort [] = []
  | mergesort [x] = [x]
  | mergesort xs =
      let
        val (ys,zs) = split xs
      in
        merge (mergesort ys, mergesort zs)
      end;

mergesort [1,3,5,2,4];
*)

fun selectsort([]) = []
  | selectsort(x::y::t : IntInf.int list) =
    if y < x then y::x::selectsort(t) else x::selectsort(y::t)
fun selectsort([a]) = [a]
  | selectsort(x::y::t : IntInf.int list) =
    if y < x then y::x::selectsort(t) else x::selectsort(y::t)
