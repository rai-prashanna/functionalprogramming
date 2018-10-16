signature Graph = 
sig 
	type ''a T 
	val empty: ''a T 
	val add_vertex: ''a T -> ''a -> ''a T
	val add_edge: ''a T -> ''a -> ''a -> ''a T
	val vertices: ''a T -> ''a list
	val neighbors: ''a T -> ''a -> ''a list
end;

structure Graph :> GRAPH =
struct
  	type ''a T = ''a list
  	val empty = []
  	fun add_vertics x xs = x::xs
  	fun add_edge x xs = x x::xs
  	fun vertices x xs = x::xs x::xs
	fun neighours x xs = x x::xs x::xs
end;

