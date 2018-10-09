(* fun iota
   TYPE: int -> a' list  
   PRE: only integer
   POST: generate the list from 0 to number enter by user
   SIDE-EFFECTS: none
*)

fun iota 0 = [] | iota n = iota(n-1) @ [n-1];

(* TYPE: 'a list * 'a list -> 'a list  
   PRE: true
   POST: generate the list of items which exist in both list
   SIDE-EFFECTS: none
*)

fun inter [] (_) = [] | inter (x::xs) ys = let fun member element []=false | member element (y::ys) = if (element=y) then true else member element ys in if member x ys then x::inter xs ys else inter xs ys end;
fun inter' (_) nil = [] | inter' nil (_) = [] | inter' (x::xs) (y::ys) = 
	if x=y then [x]@inter' xs ys else if x>y then []@inter' (x::xs) (ys) else []@inter' xs (y::ys);

(* TYPE: 'a list * 'a list -> 'a list  
   PRE: true
   POST: generate the list of items which exist in both list
   SIDE-EFFECTS: none
*)
fun inter [] (_) = [] | inter (x::xs) ys = let fun member element []=false | member element (y::ys) = if (element=y) then true else member element ys in if member x ys then x::inter xs ys else inter xs ys end;

fun inter' (_) nil = [] | inter' nil (_) = [] | inter' (x::xs) (y::ys) = if x=y then [x]@inter' xs ys else if x>y then []@inter' (x::xs) (ys) else []@inter' xs (y::ys)

(* REPRESENTATION CONVENTION: A type that models Fruit (of a given
   Apple , Banana , Lemon).

   REPRESENTATION INVARIANT: Apple should have real value;
   Banana should have real value;Lemon should have int value.
 *)
datatype Fruit = Apple of real | Banana of real | Lemon of int;

(* TYPE: Fruit list -> real   
   PRE: true
   POST: find the sum of fruit price 
   SIDE-EFFECTS: none
*)

fun sumPrice [] a b c= 0.0 | sumPrice (x::xs) a b c = case x of Apple _ => let val Apple quantity  = x in quantity*a+sumPrice xs a b c end | Banana _ => let val Banana quantity  = x in quantity*b+sumPrice xs a b c end | Lemon _ => let val Lemon quantity  = x val realquantity = Real.fromInt(quantity) 
in (realquantity*c)+sumPrice xs a b c end;

(* TYPE: 'a -> 'a * 'a list    
   PRE: true
   POST: the defination tree
   SIDE-EFFECTS: none
*)
datatype 'a btree = Node of 'a * 'a btree list;
val test_tree2 = Node (1, [Node (2, []),Node (3, [Node(7,[])]),Node (4, [])]);
val test_tree1 = Node ("hej", []);
val test_tree2 = Node (1, [Node (2, [])]);
val test_tree2 = Node (1, [Node (2, [])]);
val test_tree2 = Node (1, [Node (2, []),Node(3,[]),Node(3,[])]);

(* TYPE: 'a ltree -> int    
   PRE: true
   POST: the count of nodes in the tree
   SIDE-EFFECTS: none
*)
fun count(Node(_,[]))= 1 | count(Node(z,x::xs)) = count(x)+count(Node(z,xs));

(* TYPE: 'a ltree -> 'a list    
   PRE: true
   POST: compte the labels of nodes in the tree
   SIDE-EFFECTS: none
*)
fun labels(Node(a,[]))= [a] | labels(Node(a,x::xs))=labels(x)@labels(Node(a,xs));

(* TYPE: 'a ltree -> int   
   PRE: true
   POST: calcute the height of the tree
   SIDE-EFFECTS: none
*)
fun height (Node(_,[])) =1 | height (Node(a,x::xs)) = Int.max(height(x)+1,height(Node(a,xs)));

(* TYPE: a ltree -> ''a -> bool   
   PRE: true
   POST: check the value exist in the tree
   SIDE-EFFECTS: none
*)
fun is_present (Node(a,[])) element= if a=element then true else false | is_present (Node(a,(x::xs))) element = if a=element then true else let val Node(b,_)=x in is_present x element orelse is_present (Node(b,xs)) element end;

