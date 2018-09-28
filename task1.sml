fun avg' sum count [] = sum div count | avg' sum count (x::xs) = avg' (sum+x) (count+1) xs;
fun avg xs = avg' 0 0 xs;
val result = avg [1,2,3];