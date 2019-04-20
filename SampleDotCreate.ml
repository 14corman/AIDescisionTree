
(* The idea, and example, for this code came from here: https://github.com/rgrinberg/ocaml-depgraph/blob/master/src/depgraph.ml*)

module G = struct
	module G = Graph.Imperative.Digraph.Abstract(struct type t = string end)
	module Display = struct
		include G
		let vertex_name = V.label
		let graph_attributes _ = []
		let default_vertex_attributes _ = []
		let vertex_attributes _ = []
		let default_edge_attributes _ = []
		let edge_attributes _ = []
		let get_subgraph _ = None
	end
	module Dot = Graph.Graphviz.Dot(Display)
	include G
end

let create_root label = 
	let v = G.V.create label in
	G.add_vertex graph v

let add_child graph parent child =
	let v = G.V.create child in
	G.add_vertex graph v;
	G.add_edge graph parent v
  
let creat_graph = 
	let g = G.create() in
	let v1 = create_root "root" in
	add_child g v1 "child_1";
	add_child g v1 "child_2";

let () = G.Dot.output_graph stdout (create_graph)