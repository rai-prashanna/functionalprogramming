fun iota 0 = [] | iota n = iota(n-1) @ [n-1];


fun inter [] (_) = [] | inter (x::xs) ys = let fun member element []=false | member element (y::ys) = if (element=y) then true else member element ys in if member x ys then x::inter xs ys else inter xs ys end;
fun inter' (_) nil = [] | inter' nil (_) = [] | inter' (x::xs) (y::ys) = 
	if x=y then [x]@inter' xs ys else if x>y then []@inter' (x::xs) (ys) else []@inter' xs (y::ys);

fun inter [] (_) = [] | inter (x::xs) ys = let fun member element []=false | member element (y::ys) = if (element=y) then true else member element ys in if member x ys then x::inter xs ys else inter xs ys end;


fun inter' (_) nil = [] | inter' nil (_) = [] | inter' (x::xs) (y::ys) = if x=y then [x]@inter' xs ys else if x>y then []@inter' (x::xs) (ys) else []@inter' xs (y::ys)


datatype Fruit = Apple of real | Banana of real | Lemon of int;
fun sumPrice [] a b c= 0.0 | sumPrice (x::xs) a b c = case x of Apple _ => let val Apple quantity  = x in quantity*a+sumPrice xs a b c end | Banana _ => let val Banana quantity  = x in quantity*b+sumPrice xs a b c end | Lemon _ => let val Lemon quantity  = x val realquantity = Real.fromInt(quantity) 
in (realquantity*c)+sumPrice xs a b c end;

datatype 'a btree = Node of 'a * 'a btree list;
val test_tree2 = Node (1, [Node (2, []),Node (3, [Node(7,[])]),Node (4, [])]);
val test_tree1 = Node ("hej", []);
val test_tree2 = Node (1, [Node (2, [])]);
val test_tree2 = Node (1, [Node (2, [])]);
val test_tree2 = Node (1, [Node (2, []),Node(3,[]),Node(3,[])]);

fun count(Node(_,[]))= 1 | count(Node(z,x::xs)) = count(x)+count(Node(z,xs));

fun labels(Node(a,[]))= [a] | labels(Node(a,x::xs))=labels(x)@labels(Node(a,xs));

fun height (Node(_,[])) =1 | height (Node(a,x::xs)) = Int.max(height(x)+1,height(Node(a,xs)));


fun is_present (Node(a,[])) element= if a=element then true else false | is_present (Node(a,(x::xs))) element = if a=element then true else let val Node(b,_)=x in is_present (Node(b,xs)) element end;

