
(* append : ’a list −> ’a list −> ’a list *)
fun memeber x = foldl (fn (y,b)=>b orelse x=y) false;

val x = memeber 1 [1,2,3];


(* append : ’a list −> ’a list −> ’a list *)

fun append xs ys = foldr(fn(x,acc)=>x::acc) ys xs;

append [1,2] [3,4];


(* last : ’a list −> ’a *)
fun last (x::xs) = foldl(fn(x,acc)=>x) x xs;

last [1,2,3];

(* reverse : ’a list −> ’a list *)
fun reverse xs = foldr(fn(x,acc)=> acc @ [x]) [] xs;
reverse [1,2,3];

(*filter : (’ a −> bool) −> ’a list −> ’a list*)
fun filter F xs = foldl(fn(x,acc)=> if F x then x::acc else acc) [] xs;
filter (fn n => n > 50) [1,2,3,4,200,100,5];	

(*binary tree *)
datatype tree = Void | Node of tree * int * tree;

fun subtree a b Void = Void | subtree a b (Node(Void,label,Void)) = if (label >= a andalso label < b ) then Node(Void,label,Void) else Void | subtree a b (Node(left,label,Void)) = if (label >= a andalso label < b ) then (Node(subtree a b left,label,Void)) else subtree a b left | subtree a b (Node(Void, label,right)) = if (label >= a andalso label < b ) then (Node(Void,label,subtree a b right)) else subtree a b right | subtree a b (Node (left,label,right))= if (label >= a andalso label < b ) then Node(subtree a b left,label,subtree a b right) else subtree(Node(subtree a b left,label,subtree a b right));



fun subtree a b Void = Void | subtree a b (Node(Void,label,Void)) = if (label >= a andalso label < b ) then Node(Void,label,Void) else Void | subtree a b (Node (left,label,right))= if (label >= a andalso label < b ) then Node(subtree a b left,label,subtree a b right) else subtree a b (Node(subtree a b left,label,subtree a b right));
 

	| subtree a b (Node(left,label,Void)) = if (label >= a andalso label < b ) then (Node(subtree a b left,label,Void)) else subtree a b left ;
	| subtree a b (Node(Void, label,right)) = if (label >= a andalso label < b ) then (Node(Void,label,subtree a b right)) else subtree a b right 
	| subtree a b (Node (left,label,right))= if (label >= a andalso label < b ) then Node(subtree a b left,label,subtree a b right) else subtree(Node(subtree a b left,label,subtree a b right));







val ex1 = Node(Node(Node(Void, 0, Node(Void, 2, Void)), 3, Node(Void, 5, Void)), 6, Node(Void, 7, Node(Void, 8, Node(Void, 9, Node(Node(Void, 10, Void), 12, Node(Void, 15, Node(Void, 19, Void)))))))

val ex1 = Node(Void,6,Void);

subtree 5 8 ex1;