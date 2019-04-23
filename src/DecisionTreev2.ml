open Graph

module EDGE = struct
  type t = int
  let compare : t -> t -> int = Pervasives.compare
  let default = -1
end

module DTree = struct
  exception Var_attribute_mismatch
  module G = Graph.Imperative.Digraph.AbstractLabeled(struct type t = string end)(EDGE)
  module Display = struct
    include G
    let vertex_name = V.label
    let edge_name = E.label
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

  let print_graph = Dot.output_graph stdout g

  (* Need to change label = 1 to something else at some point. *)
  let rec remainder vars attr labels =
    if attr = -1 then 0.0 else 
      let (pi, ni) = (List.fold_left2 (fun a var label -> if var = attr then (if label = 1 then (fst a + 1, snd a) else (fst a, snd a + 1)) else a) (0, 0) vars labels) in
      let pi_ni = pi + ni in
      let pi_pini = pi / pi_ni in
      let ni_pini = ni / pi_ni in
      let result = float_of_int (pi_ni / (List.length labels)) *. (-. float_of_int (pi_pini) *. (log (float_of_int pi_pini)) -. (float_of_int ni_pini) *. (log (float_of_int ni_pini))) in
      (remainder vars (attr - 1) labels) +. result

  (* Return: (The winning feature row, the winnin feature's attribute number, remainder of feature, list of rest of features, list of rest of attributes) *)
  let choose_var x y attrs = 
    Feature_map.fold (fun key value a -> let remainderVar = remainder value (Feature_map.find key attrs) y in 
                       if remainderVar < snd a then (key, remainderVar) else a) x ("", 100000000.0)

  let build_tree_v2 x_map y attr_map =
    let partition_x (attr : int) x = Feature_map.fold (fun key value_list a -> 
        Feature_map.add key (fst (List.partition (fun value -> value = attr) value_list)) a) x Feature_map.empty in
    let partition_y attr x_val y = List.fold_right2 (fun val_x val_y a -> if val_x = attr then val_y::a else a) x_val y [] in
    let rec build_loop_list = function
      | -1 -> []
      | attr -> attr::(build_loop_list (attr - 1)) in
    let rec df_build x y attrs parent parent_attr depth =
      let classes = List.sort_uniq (fun x y -> if x > y then 1 else if x = y then -1 else 0) y in
      if (Feature_map.cardinal x) = 0 || (List.length classes) <= 1 then
        begin
          let v = G.V.create (string_of_int (List.hd classes)) in
          G.add_vertex g v;
          let e = G.E.create parent parent_attr v in
          G.add_edge_e g e
        end 
      else
        begin
          if G.V.label parent != "tmp" then
            begin
              let winner = choose_var x y attrs in
              let v = G.V.create (fst winner ^ "<br/>" ^ (string_of_float(snd winner))) in
              G.add_vertex g v;
              let e = G.E.create parent parent_attr v in
              G.add_edge_e g e;
              let attr_list = build_loop_list (Feature_map.find (fst winner) attrs) in
              let new_attrs = Feature_map.remove (fst winner) attrs in
              let winning_var_list = Feature_map.find (fst winner) x in
              let remove_winner = Feature_map.remove (fst winner) x in
              List.iter (fun a -> df_build (partition_x a remove_winner) (partition_y a winning_var_list y) new_attrs v a (depth + 1)) attr_list 
            end
          else 
            begin
              let winner = choose_var x y attrs in
              let v = G.V.create (fst winner ^ "<br/>" ^ (string_of_float(snd winner))) in
              G.add_vertex g v;
              let attr_list = build_loop_list (Feature_map.find (fst winner) attrs) in
              let new_attrs = Feature_map.remove (fst winner) attrs in
              let winning_var_list = Feature_map.find (fst winner) x in
              let remove_winner = Feature_map.remove (fst winner) x in
              List.iter (fun a -> df_build (partition_x a remove_winner) (partition_y a winning_var_list y) new_attrs v a (depth + 1)) attr_list 
            end
        end
    in df_build x_map y attr_map (G.V.create "tmp") 0 1
end

