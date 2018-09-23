
 

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


(* Question2 *)
fun split [] =
      ([], [])
  | split [x] =
      ([x], [])
  | split (x1::x2::xs) =
      let
        val (xs1,xs2) = split xs
      in
        (x1::xs1, x2::xs2)
      end;

split [1,2,3,4,5];

(* merge (xs,ys)
   TYPE: int list * int list -> int list
   PRE: xs and ys are sorted
   POST: a sorted permutation of xs @ ys
 *)
fun merge ([], ys) = ys
  | merge (xs, []) = xs
  | merge (x::xs, y::ys) =
      if x <= y then
        x :: merge (xs, y::ys)
      else
        y :: merge (x::xs, ys);

merge ([1,3,5], [2,4]);

(* mergesort xs
   TYPE: int list -> int list
   PRE: true
   POST: a sorted permutation of xs
 *)
fun mergesort [] = []
  | mergesort [x] = [x]
  | mergesort xs =
      let
        val (ys,zs) = split xs
      in
        merge (mergesort ys, mergesort zs)
      end;

mergesort [1,3,5,2,4];


fun intersect ([], _) = []
| intersect (x::xs, ys) =
  if List.exists (fn y => x = y) ys
  then x :: intersect (xs, ys)
  else intersect (xs, ys);

val l1 = [1,2,3];
val l2 = [1,2,4];

intersect(l1,l2);
