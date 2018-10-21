signature GRAPH = 
sig 
	type ''a T
  val empty: ''a T
  val add_vertex: ''a T -> ''a  -> ''a T
  val add_edge: ''a T -> ''a -> ''a ->''a T
  val vertices: ''a T -> ''a list
end;


*****

*********
structure Graph : GRAPH =
struct
datatype ''a component = Void | Point of (''a ) | Connected of (''a component * (''a component) list) list | Edge of (''a component * ''a component);
type ''a T = ''a component 
val empty = Void
fun add_vertex Void (a) = Point(a)
  | add_vertex (Point(a)) (b)=Connected([(Point(a),[]),(Point(b),[])])
  | add_vertex (Connected(xs)) (b)=Connected([(Point(b),[])] @ xs);
fun addComponent (Connected([])) (Connected([])) = Connected([]) | addComponent (Connected([])) (Connected(x::xs)) = Connected(x::xs) 
  | addComponent (Connected(xs)) (Connected(ys)) = Connected(xs@ys);

fun add_edge Void point1 point2 =Void
  | add_edge (Point(a)) point1 point2 = (Point(a))
  | add_edge (Connected(x::xs)) point1 point2 =  
  let 
    val (point,edges) = x
    in (Connected([(point,[Edge(Point(point1),Point(point2))]@edges)])) end;

end;

