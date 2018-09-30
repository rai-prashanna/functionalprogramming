
fun inter [] (_) = [] | inter (x::xs) ys = let fun member element []=false | member element (y::ys) = if (element=y) 
	then true else member element ys in if member x ys then x::inter xs ys else inter xs ys end;

fun inter' (_) nil = [] | inter' (x::xs) (y::ys) = if x=y then x::inter' xs ys else if x>y then inter' (x::xs) ys else inter' xs (y::ys);


fun inter' (_) nil = [] | inter' nil (_) = [] | inter' (x::xs) (y::ys) = if x=y then [x]@inter' xs ys else if x>y then []@inter' (x::xs) (ys) else []@inter' xs (y::ys);
inter' [1,2,3,4] [0,1,7];
inter' [0,1,7] [0,1];
inter' [1, 2, 3, 4] [2, 4, 5];


