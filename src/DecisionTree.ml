open Graph

module DTree = struct
  exception Var_attribute_mismatch
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

  let g = G.create ()

  (* Need to change label = 1 to something else at some point. *)
  let rec remainder vars attr labels =
    if attr = -1 then 0.0 else 
      let (pi, ni) = (List.fold_left2 (fun a var label -> if var = attr then (if label = 1 then (a.fst + 1, a.snd) else (a.fst, a.snd + 1)) else a) (0, 0) vars labels) in
      let pi_ni = pi + ni in
      let pi_pini = pi / pi_ni in
      let ni_pini = ni / pi_ni in
      let result = float_of_int (pi_ni / (List.length labels)) *. (-. float_of_int (pi_pini) *. (log (float_of_int pi_pini)) -. (float_of_int ni_pini) *. (log (float_of_int ni_pini))) in
      (remainder vars (attr - 1) labels) +. result

  (* Return: (The winning feature row, the winnin feature's attribute number, remainder of feature, list of rest of features, list of rest of attributes) *)
  let rec choose_var x y attrs = match (x, attrs) with
    | ([], []) -> ([], 0, 10000000.0, [], [])
    | ([], attr::t2) -> raise Var_attribute_mismatch
    | (var::t1, []) -> raise Var_attribute_mismatch
    | (var::t1, attr::t2) -> 
      let remainderVar = remainder var attr y in 
      let (old_win, old_attr, old_remainder, old_list, old_attrs) = choose_var t1 y t2 in
      if remainderVar > old_remainder 
      then (old_win, old_attr, old_remainder, var::old_list, attr::old_attrs) else (var, attr, remainderVar, old_win::old_list, old_attr::old_attrs)


  (* 
    xs = 3d array of values where the shape is (number of partitions, number of x's, number of examples). An example could be (1, 5, 600).
    For xs, each call to build_layer will partition xs into m number of partitions where m is the number of attributes the winning variable at that
    position has. So, if the winning variable the first time (root of tree) has 3 partitions then you might have (3, 4, n) where n is the number of examples
    per partition.

    ys = similar to x, but it only is 2d. For example, say we look at the same example from xs: (1, 600).

    attrs = a 1d list of atributes for each feature (x). Say you have a feature that is boolean, then its attribute will be 1 (for 1 and 0). If you have a 
    category, for example yes, no, and maybe, then it will have an attribute of 2 (0, 1, or 2). If you put these 2 together then their attribute list would 
    be [1; 2].

    All 3 varaibles MUST be in the same order. The (_, i, _) part of xs must match (_, i) of ys, and [i] of attrs.

  *)
  let build_tree xs ys attrs = 
    let rec partition node_vars x attr = match attr with
      | -1 -> []
      | _ -> let (with_attr, wo_attr) = List.fold_left2 (fun a var x_part -> if var = attr then (x_part::a.fst, a.snd) else (a.fst, x_part::a.snd)) ([], []) node_vars x in
        with_attr::(partition node_vars wo_attr (attr - 1)) in
    let rec build_layer xs ys = match (xs, ys) with
      | ([], []) -> (* Print out tree *)...
      | ([], y::t2) -> raise Var_attribute_mismatch
      | (x::t1, []) -> raise Var_attribute_mismatch
      | (x::t2, y::t2) -> let (var, attr, remainderVar, new_x, new_attrs) = choose_var x y attrs in 
        let partitions = partition var new_x attr in
        let filtered_partitions = List.filter (fun a -> a != []) partition
        (* Not sure what to do with partitions. Need to know what the previous Node is so when you make the new nodes you can create the edges. You need to do a 
           depth first search of sorts to make this work right. *)


  (* Maybe should try to use maps for x and y. The map for x would have the name of the feature be the key so you can use to look it up later and name the node.
     You could use a map for y by having the key be an integer of the number of example so while going over the x's you can simply look up what the label is where
     you are at. *)



end

