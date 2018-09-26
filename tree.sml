datatype 'a btree = Empty | Node of 'a btree * 'a * 'a btree;
fun countNodes(Empty)= 0 | countNodes(Node(left,a,right)) = 1 + countNodes(left) + countNodes(right);
val mytree = Node(Node(Empty,2,Empty),1,Node(Node(Empty,4,Empty),3,Node(Empty,6,Empty)));
countNodes(mytree);

fun listNodes(Empty) = nil | listNodes(Node(left,x,right)) = [x] @ listNodes(left) @ listNodes(right);
listNodes(mytree);
fun is_present(Empty) item= false | is_present(Node(left,x,right)) item= if x=item then true else is_present(left) item orelse is_present(right) item;
is_present(mytree) 1;
is_present(mytree) 5;

fun height(Empty) = 0 | height(Node(left,_,right)) = 1 + Int.max(height(left) ,height(right)); 
height(mytree);	