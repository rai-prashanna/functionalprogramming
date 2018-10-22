signature GRAPH = 
sig 
  type ''a T
  val empty: ''a T
  val add_vertex: ''a T -> ''a  -> ''a T
  val add_edge: ''a T -> ''a -> ''a ->''a T
  val vertices: ''a T -> ''a list
  val neighbors: ''a T -> ''a -> ''a list
end;


structure Graph : GRAPH =
struct
datatype ''a component = Void | Point of (''a ) | Connected of (''a component * (''a component) list) list | Edge of (''a component * ''a component);
type ''a T = ''a component 
val empty = Void
fun add_vertex Void (a) = Point(a)
  | add_vertex (Point(a)) (b)=Connected([(Point(a),[]),(Point(b),[])])
  | add_vertex (Connected(xs)) (b)=Connected([(Point(b),[])] @ xs)

fun addComponent (Connected([])) (Connected([])) = Connected([]) | addComponent (Connected([])) (Connected(x::xs)) = Connected(x::xs) 
  | addComponent (Connected(xs)) (Connected(ys)) = Connected(xs@ys);

fun add_edge Void point1 point2 = (Connected([]))
  | add_edge (Point(a)) point1 point2 = (Connected([]))
  | add_edge (Connected([])) point1 point2 =(Connected([]))
  | add_edge (Connected(x::xs): ''a component) (point1:''a) (point2:''a) =  
  let 
    val (point , edges) = x
    val (Point(value)) = point
    val pointOne =Point(point1)
    val pointTwo =Point(point2)
    in 
      if (value=point1 orelse value=point2) then addComponent (Connected([(point,[Edge(Point(point1),Point(point2))]@edges)])) (add_edge (Connected(xs)) point1 point2) 
    else (addComponent (Connected([x])) (add_edge (Connected(xs)) point1 point2))
  end

fun getPointsExcluding ([]) element= [] 
| getPointsExcluding ((Edge (Point(value1), Point(value2)))::xs) element= if (value1 <> element) then [value1] @ getPointsExcluding (xs) element else [value2] @ getPointsExcluding (xs) element;


fun vertices Void = []
| vertices (Point(a)) = [a]
| vertices (Connected([])) =[]
| vertices (Connected(x::xs)) =
  let
    val (point , edges) = x
    val (Point(value)) = point
    in
    [value] @ (vertices(Connected(xs)))
end

fun neighbors Void element = []
| neighbors (Point(a)) element = []
| neighbors (Connected([])) element = []
| neighbors (Connected((Point(a),[])::xs)) element = neighbors (Connected(xs)) element
| neighbors (Connected((Point(a),ys)::xs)) element = if (a=element) then getPointsExcluding (ys) element else neighbors (Connected(xs)) element
end;


val g1 = Graph.add_vertex (Graph.add_vertex (Graph.add_vertex Graph.empty "a") "b") "c";
val g2 = Graph.add_edge (Graph.add_edge (Graph.add_edge g1 "a" "b") "a" "c") "b" "c";
val g3 = Graph.add_edge (Graph.add_edge (Graph.add_edge g1 "b" "a") "c" "a") "c" "b";
Graph.vertices Graph.empty;
Graph.vertices (Graph.add_vertex Graph.empty "a");
Graph.vertices g1;
Graph.vertices g2;
Graph.vertices g3;
Graph.neighbors g1 "a";
Graph.neighbors g2 "a";
Graph.neighbors g3 "a";
