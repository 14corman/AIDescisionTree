open Graph;;

module EDGE = struct
  type t = int
  let compare : t -> t -> int = Pervasives.compare
  let default = -1
  type label = string
end;;

module DTree = struct
  exception Var_attribute_mismatch
  module G = Graph.Imperative.Digraph.AbstractLabeled(struct type t = string end)(EDGE)
  module Display = struct
    include G
    (* https://b0-system.github.io/odig/doc/ocamlgraph/Graph/Graphviz/DotAttributes/index.html *)
    let vertex_name = V.label
    let edge_name = E.label
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_attributes _ = [`Shape `Box; `Style `Filled; `Style `Rounded]
    let default_edge_attributes _ = []
    let edge_attributes _ = [`Color 4711]
    let get_subgraph _ = None
  end
  module Dot = Graph.Graphviz.Dot(Display)
  module Feature_map = Map.Make(String)

  let g = G.create ()

  let size = ref 0

  let print_graph = Dot.output_graph stdout g

  (* Need to change label = 1 to something else at some point. *)
  let rec remainder vars attr labels =
    if attr = -1 then 0.0 else 
      let (pi, ni) = (List.fold_left2 (fun a var label -> if var = attr then (if label = 1 then ((fst a) +. 1.0, snd a) else (fst a, (snd a) +. 1.0)) else a) (0.0, 0.0) vars labels) in
      (*Printf.printf "Pi: %f, " (pi); Printf.printf "Ni: %f\n" (ni);*)
      let pi_ni = pi +. ni +. 0.0000001 in
      (*Printf.printf "Pi_ni: %f\n" (pi_ni);*)
      let pi_pini = pi /. pi_ni +. 0.0000001 in
      (*Printf.printf "pi_pini: %f\n" (pi_pini);*)
      let ni_pini = ni /. pi_ni +. 0.0000001 in
      (*Printf.printf "ni_pini: %f\n" (ni_pini);*)
      let result = (pi_ni /. (float_of_int (List.length labels))) *. ((0.0 -. pi_pini) *. (log pi_pini) -. ni_pini *. (log ni_pini)) in
      (*Printf.printf "result: %f\n\n" (result);*)
      (remainder vars (attr - 1) labels) +. result;;

  let print_list vals = List.fold_left (fun a value -> a ^ "," ^ (string_of_int value)) "" vals;;

  (* Return: (The winning feature row, the winnin feature's attribute number, remainder of feature, list of rest of features, list of rest of attributes) *)
  let choose_var x y attrs = 
    Feature_map.fold (fun key value a -> Printf.printf "Choose var (key) (value) (a) (y) (attr) : (%s) (%s) ((%s, %f)) (%s) (%i)\n\n" (key) (print_list value) (fst a) (snd a) (print_list y) (Feature_map.find key attrs);
                       let remainderVar = remainder value (Feature_map.find key attrs) y in
                       if remainderVar < (snd a) then (key, remainderVar) else a) x ("", 100000000.0)

  let build_tree_v2 x_map y attr_map =
    let partition_x (attr : int) win_x x = Feature_map.fold (fun key value_list a -> 
        Feature_map.add key (List.fold_right2 (fun value win_val a -> if win_val = attr then value::a else a) value_list win_x []) a) x Feature_map.empty in
    let partition_y attr x_val y = List.fold_right2 (fun val_x val_y a -> if Printf.printf "Partition y: (attr) (val_x) (val_y) : (%i) (%i) (%i)\n" (attr) (val_x) (val_y);
                                                      val_x = attr then val_y::a else a) x_val y [] in
    let rec build_loop_list = function
      | -1 -> []
      | attr -> attr::(build_loop_list (attr - 1)) in
    let rec df_build x y attrs parent parent_attr depth =
      Printf.printf "Depth: %i\n\n" (depth);
      let classes = List.sort_uniq (fun x y -> if x > y then 1 else if x = y then -1 else 0) y in
      if (List.length classes) <= 0 then
        Printf.printf "Do not know what to do here...\n"
      else
        begin
          if (Feature_map.cardinal x) = 0 && List.length classes >= 1 then
            begin
              let v = G.V.create (string_of_int !size(*(List.hd classes)*)) in
              size := !size + 1;
              G.add_vertex g v;
              let e = G.E.create parent parent_attr v in
              G.add_edge_e g e
            end 
          else
            begin
              let winner = choose_var x y attrs in
              Printf.printf "Winner = %s" (fst winner); Printf.printf ": %f\n" (snd winner);
              let attr_list = build_loop_list (Feature_map.find (fst winner) attrs) in
              let new_attrs = Feature_map.remove (fst winner) attrs in
              let winning_var_list = Feature_map.find (fst winner) x in
              let remove_winner = Feature_map.remove (fst winner) x in
              size := !size + 1;
              let v = G.V.create (string_of_int !size);(*(fst winner ^ "<br/>" ^ (string_of_float(snd winner)))*) in
              G.add_vertex g v;
              if !size != 0 then
                begin
                  let e = G.E.create parent parent_attr v in
                  G.add_edge_e g e;
                end;
              List.iter (fun a -> df_build (partition_x a winning_var_list remove_winner) (partition_y a winning_var_list y) new_attrs v a (depth + 1)) attr_list 
            end
        end
    in df_build x_map y attr_map (G.V.create "-1") 0 1;  Dot.output_graph stdout g
end

