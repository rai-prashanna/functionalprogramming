datatype 'a btree = Node of 'a * 'a btree list;
val test_tree3 = Node ("hej",
                                   [Node ("hello", [Node ("ni hao", [Node ("ahoj", [])])]),
                                    Node ("bonjour", [Node ("privet", [Node ("guten tag", [])])]),
                                    Node ("namaste", [Node ("ciao", [Node ("As-salam alaykom", [Node ("saluton", [Node ("hei", [Node ("halo", [])])]),
                                                                                                Node ("kon-nichiwa", [Node ("an-nyong ha-se-yo ", [Node ("ola", [])])]),
                                                                                                Node ("sa-wat-dee", [Node ("selam", [Node ("jambo", [])])])])])])]);


fun height (Node(_,[])) =1 | height (Node(a,x::xs)) = Int.max(height(x)+1,height(Node(a,xs)));


val test_tree5 = Node(1,[Node(2,[Node(3,[]),Node(4,[]),Node(5,[Node(6,[])])]),Node(7,[])]); 



val test_tree0= Node(1,[Node(2,[Node(3,[]),Node(4,[]),Node(5,[Node(6,[])])]),Node(7,[])]);
height test_tree5;




height test_tree3;
