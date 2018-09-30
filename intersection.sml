fun member element []= false | member element (x::xs) = if (element=x) then true else member element xs;
val result= member 2 [1,3,4,2];

fun inter [] (_) = [] | inter (x::xs) ys = if member x ys then x::inter xs ys else inter xs ys; 




