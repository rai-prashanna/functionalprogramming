datatype 'a btree = Empty | Node of 'a btree * 'a * 'a btree;
fun countNodes(Empty)= 0 | countNodes(Node(left,a,right)) = 1 + countNodes(left) + countNodes(right);
val mytree = Node(Node(Empty,2,Empty),1,Node(Node(Empty,4,Empty),3,Node(Empty,6,Empty)));
countNodes(mytree);

fun listNodes(Empty) = nil | listNodes(Node(left,x,right)) = [x] @ listNodes(left) @ listNodes(right);
