(* append : ’a list −> ’a list −> ’a list *)

fun foldr f b [] = [b] | foldr f b (xs) = f (xs, [b]);
fun append a xs = foldr (op @) a xs;
append 2 [1,3];


