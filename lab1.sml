(*******************************************************************************************)
(*******************************************************************************************)
(* Question 1: Reduction, Specification and Variant 
fun product n =
if n = 1 then 1
else n ∗ product (n−1)
	*)


(* Question 1.1: Step-by-Step Evaluation
	product 3
	*)

(* product 3
--> if 3 = 1 then 1 else 3 * product(3 - 1);
--> if false then 1 else 3 * product(3-1);
--> 3 * product(3 - 1);
--> 3 * product 2;
--> 3 * (if 2 = 1 then 1 else 2 * product(2 - 1));
--> 3 * (if false then 1 else 2 * product(2 - 1));
--> 3 * (2 * product(2 - 1));
--> 3 * (2 * product 1);
--> 3 * (2 * (if 1 = 1 then 1 else 1 * product(1 - 1)));
--> 3 * (2 * (if true then 1 else 1 * product(1 - 1)));
--> 3 * (2 * 1);
--> 3 * 2;
--> 6 *)

(* Question 1.2: What does the function compute?
	*)
(* factorial of n  *)


(* Question 1.3:  Specification for the function
	*)
(* product n
   TYPE: int -> int
   PRE: n>=0
   POST: n!
   SIDE-EFFECTS: none
   EXAMPLES: product 1 = 1, product 3 = 6
*)

(*******************************************************************************************)
(*******************************************************************************************)
(* Question 2: Currying 
fun minus x y = x − y;
	*)

(* Question 2.1:  function declaration as a value declaration val minus = *)
val minus = fn x => fn y => x- y;

(* Question 2.2:  val foo = minus 5 4 *)
(* val foo = 1: int *)


(*Question 2.3: val bar = minus 5 *)
(* val bar = fn: int -> int *)

(*Question 2.4: step-by-step evaluation of minus 5 4 *)
(*  *)
 

(*******************************************************************************************)
(*******************************************************************************************)
(* Question 3: Types 
	*)
(* Question 3.1: int −> int *)
(* fun1 x
   TYPE: int -> int
   PRE: only integer
   POST: x+1
   SIDE-EFFECTS: none
   EXAMPLES: fun1 1 = 2, fun1 3 = 4
*)
 fun fun1 x = x+1;


(* Question 3.2: int −> int −> int *)
(* fun2 x y
   TYPE: int −> int −> int
   PRE: only integer
   POST: x+y
   SIDE-EFFECTS: none
   EXAMPLES: fun1 1 1 = 2, fun1 3 1 = 4
*)
fun fun2 x y = x+y;

(* Question 3.3: int −> int ∗ int *)
(* fun3 x
   TYPE: int −> int ∗ int
   PRE: only integer
   POST: (2*x,3*x)
   SIDE-EFFECTS: none
 *)

fun fun3 x = (2*x,3*x);

(* Question 3.4: int ∗ int −> int *)
(* fun4 (x,y)
   TYPE: int ∗ int −> int
   PRE: only integer
   POST: x*y
   SIDE-EFFECTS: none
 *)
fun fun4 (x,y) = x*y;


(* Question 3.5: int −> real −> string −> string *)
(* fun5 x y z
   TYPE: int −> real −> string −> string
   PRE: true
   POST: returns z 
   SIDE-EFFECTS: none
 *)
fun fun5 (x:int) (y:real) (z:string) = if z="foo" then "bar" else z;

(* Question 3.6: int ∗ ( string ∗ string ∗ int ) −> int ∗ string *)
(* fun6 x y z w
   TYPE: int ∗ ( string ∗ string ∗ int ) −> int ∗ string
   PRE: only integer
   POST: convert integer valuse of x to string 
   SIDE-EFFECTS: none
 *)  
fun fun6 (x:int,(y:string,z:string,w:int)) =  (x,Int.toString(w));


(*******************************************************************************************)
(*******************************************************************************************)
(* Question 4: Sum-Square Difference *)
(* sum_square_diff n
   TYPE: int -> int
   PRE: only positive integer
   POST: calcuates difference between square of sum of all number up to n with sum of squares of numbers upto n
   SIDE-EFFECTS: none
 *)  
fun sumofsquares n = if n=1 then 1 else n*n + sumofsquares(n-1);
fun squareofsum n : int = (n*n*(n+1)*(n+1)) div 4;
exception Matherror ;
fun sum_square_diff n = if n < 1 then raise Matherror else squareofsum n - sumofsquares n;

(*******************************************************************************************)
(*Submitted By,                                                                            *)
(*G 13                                                                                     *)
(*Prashanna Rai                                                                            *)
(*Farooq Cheemai                                                                           *)
(*******************************************************************************************)




