
(* The idea, and example, for this code came from here: https://github.com/rgrinberg/ocaml-depgraph/blob/master/src/depgraph.ml*)

open Graph

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

let create_root graph label = 
  let v = G.V.create label in
  let () = G.add_vertex graph v in
  v

let add_child graph parent child =
  let v = G.V.create child in
  G.add_vertex graph v;
  G.add_edge graph parent v

let g = G.create ()
let v1 = create_root g "root"
let () = add_child g v1 "child_1"
let () = add_child g v1 "child_2" 

let () = G.Dot.output_graph stdout g