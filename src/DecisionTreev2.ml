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
  module Feature_map = Map.Make(String)
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
  let choose_var x y attrs = 
    Feature_map.fold (fun key value a -> let remainderVar = remainder value (Feature_map.find key attrs) y in 
                       if remainderVar < a.snd then (key, remainderVar) else a) x ("", 100000000.0)

  let build_tree_v2 x_map y attr_map =
    let partition_x attr x = Feature_map.fold (fun key value_list a -> let partitioned_values = List.partition (fun val -> val = attr) value_list in
                                                Feature_map.add key partitioned_values.fst a) x Feature_map.empty in 
    let partition_y attr x_val y = List.fold_right2 (fun a val_x val_y -> if val_x = attr then val_y::a) [] x_val y in
    let rec build_loop_list = function
      | -1 -> []
      | attr -> attr::(build_loop_list (attr - 1)) in
    let rec df_build x y attrs parent parent_attr depth = 
      if (Feature_map.cardinal x) = 0 || (* Check if y list has only 1 type of value. Perhaps use an entropy function and check if that is 0.*)true then
        (* At leaf node. Use y values to determine what the node should be.*)()
      else
        begin
          if parent != None then
            begin
              let winner = choose_var x y attrs in
              let v = G.V.create (winner.fst * "<br/>" * winnder.snd) in
              G.add_vertex g v;
              let e = G.E.create parent parent_attr v in
              G.add_edge_e g e;
              let attr_list = build_loop_list (Feature_map.find attrs winner.fst) in
              let new_attrs = Feature_map.remove attrs winner.fst in
              let winning_var_list = Feature_map.find x winner.fst in
              let remove_winner = Feature_map.remove x winner.fst in
              List.iter (fun a -> df_build (partition_x a remove_winner) (partition_y a winning_var_list y) new_attrs v a (depth + 1)) attr_list 
            end
          else 
            begin
              let winner = choose_var x y attrs in
              let v = G.V.create (winner.fst * "<br/>" * winnder.snd) in
              G.add_vertex g v;
              let attr_list = build_loop_list (Feature_map.find attrs winner.fst) in
              let new_attrs = Feature_map.remove attrs winner.fst in
              let winning_var_list = Feature_map.find x winner.fst in
              let remove_winner = Feature_map.remove x winner.fst in
              List.iter (fun a -> df_build (partition_x a remove_winner) (partition_y a winning_var_list y) new_attrs v a (depth + 1)) attr_list 
            end
        end
    in df_build x_map y attr_map None 0 1
end

