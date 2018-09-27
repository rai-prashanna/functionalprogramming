datatype Fruit = Apple of real | Banana of real | Lemon of int;

val myapple= Apple 2.0;
val mybanana= Banana 2.0;
val mylemon= Lemon 2;

val listFruit= [Apple 2.0, Banana 2.0];


fun sumPrice [] a b c= 0.0 | sumPrice (x::xs) a b c = case x of Apple _ => let val Apple quantity  = x in quantity*a+sumPrice xs a b c end | Banana _ => let val Banana quantity  = x in quantity*b+sumPrice xs a b c end;


sumPrice listFruit 2.0 3.0 2.0;