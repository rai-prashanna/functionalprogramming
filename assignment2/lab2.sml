(* iota n
   TYPE: int -> a' list  
   PRE: only positive integer
   POST: generate the list from 0 to n-1 enter by user
   SIDE-EFFECTS: none
*)
(* VARIANT: n *)

fun iota 0 = [] | iota n = iota(n-1) @ [n-1];


(* inter list1 list2
   TYPE: 'a list * 'a list -> 'a list  
   PRE: true
   POST: generate the list of items which exist in both list
   SIDE-EFFECTS: none
*)
(* VARIANT: list1 list2 *)

(* member element list
   TYPE: ''a -> ''a list -> bool
   PRE: true
   POST: checks whether element is present in list
   SIDE-EFFECTS: none
*)
(* VARIANT: list *)

fun inter [] (_) = [] | inter (x::xs) ys = 
let fun member element []=false | member element (y::ys) = if (element=y) then true else member element ys 
in if member x ys then x::inter xs ys else inter xs ys end;

(* inter' list1 list2
   TYPE: int list -> int list -> int list  
   PRE: any list of integer
   POST: generate the list of items which exist in both list
   SIDE-EFFECTS: none
*)
(* VARIANT: list1 list2 *)

fun inter' (_) nil = [] | 
inter' nil (_) = [] | 
inter' (x::xs) (y::ys) = 
if x=y then x::inter' xs ys else if x>y then inter' (x::xs) (ys) else inter' xs (y::ys)

(* REPRESENTATION CONVENTION: A type that models Fruit (of a given
   Apple , Banana , Lemon).

   REPRESENTATION INVARIANT: Apple should have real value;
   Banana should have real value;Lemon should have int value.
 *)
datatype Fruit = Apple of real | Banana of real | Lemon of int;

(* sumPrice list a b c
   TYPE: Fruit list -> real -> real -> real -> real
   PRE: true
   POST: find the sum of fruit price 
   SIDE-EFFECTS: none
*)
fun sumPrice [] a b c= 0.0 | sumPrice (x::xs) a b c = case x of Apple quantity => quantity*a+sumPrice xs a b c | Banana quantity => quantity*b+sumPrice xs a b c | Lemon quantity => Real.fromInt(quantity)*c+sumPrice xs a b c; 


(* REPRESENTATION CONVENTION: A type that models tree.
   REPRESENTATION INVARIANT: It consists of nested node where each node contains tuple of 
   label(String) and list of self refrencing node
 *)
datatype 'a btree = Node of 'a * 'a btree list;

(* count ltree
   TYPE: 'a ltree -> int    
   PRE: true
   POST: the count of nodes in the tree
   SIDE-EFFECTS: none
*)
(* VARIANT: size of ltree *)
fun count(Node(_,[]))= 1 | count(Node(z,x::xs)) = count(x)+count(Node(z,xs));

(* labels ltree
   TYPE: 'a ltree -> 'a list    
   PRE: true
   POST: compute the labels of nodes in the tree
   SIDE-EFFECTS: none
*)
(* VARIANT: size of ltree *)
fun labels(Node(a,[]))= [a] | labels(Node(a,x::xs))=labels(x)@labels(Node(a,xs));

(* height ltree
   TYPE: 'a ltree -> int   
   PRE: true
   POST: calcute the height of the tree
   SIDE-EFFECTS: none
*)
(* VARIANT: size of ltree *)
fun height (Node(_,[])) =1 | height (Node(a,x::xs)) = Int.max(height(x)+1,height(Node(a,xs)));

(* is_present ltree element
   TYPE: a ltree -> ''a -> bool   
   PRE: true
   POST: check the value exist in the tree
   SIDE-EFFECTS: none
*)
(* VARIANT: size of ltree *)
fun is_present (Node(a,[])) element= if a=element then true else false | is_present (Node(a,(x::xs))) element = if a=element then true else let val Node(b,_)=x in is_present x element orelse is_present (Node(b,xs)) element end;

