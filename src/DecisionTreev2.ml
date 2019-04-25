open Graph

module EDGE = struct
  type t = int
  let compare : t -> t -> int = Pervasives.compare
  let default = -1
end

module DTree = struct
  exception Var_attribute_mismatch
  module G = Graph.Imperative.Digraph.AbstractLabeled(struct type t = string * string end)(EDGE)
  module Display = struct
    include G
    (* https://b0-system.github.io/odig/doc/ocamlgraph/Graph/Graphviz/DotAttributes/index.html *)
    let vertex_name v =  fst (V.label v)
    let edge_name = E.label
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_attributes v = [`Shape `Box; `Style `Filled; `Style `Rounded; `HtmlLabel (snd (V.label v))]
    let default_edge_attributes _ = []
    let edge_attributes e = [`Color 4711; `Label (string_of_int (E.label e))]
    let get_subgraph _ = None
  end
  module Dot = Graph.Graphviz.Dot(Display)
  module Feature_map = Map.Make(String)

  let g = G.create ()

  (* The number of vertices in the graph. Will be used to make vertex ids later.*)
  let size = ref 0

  (* Used to print the graph out to standard out.*)
  let print_graph g = Dot.output_graph stdout g

  (* Calculates the entropy of a given list of labels.*)
  let entropy ys = 
    let log2 = 1.0 /. log 2.0 in
    let (p, n) = List.partition  (fun y -> y = 1) ys in
    Printf.printf "p: %i, n: %i\n" (List.length p) (List.length n);
    let num_p = float_of_int (List.length p) in
    let num_n = float_of_int (List.length n) in
    let combined = num_p +. num_n +. 0.0000000000000001 in
    let p_combined = num_p /. combined +. 0.0000000000000001 in Printf.printf "Positive division: %f\n" (p_combined);
    let n_combined = num_n /. combined +. 0.0000000000000001 in Printf.printf "Negative division: %f\n" (n_combined);
    let entropy_val = (0.0 -. p_combined) *. ((log p_combined) *. log2) -. n_combined *. ((log n_combined) *. log2) in
    Printf.printf "Entropy value: %f\n" (entropy_val); entropy_val

  (* Need to change label = 1 to something else at some point if we need to do multiclass classification.
     Computes the remainder of labels and attributes. *)
  let rec remainder vars attr labels =
    if attr = -1 then 0.0 else 
      let new_y = (List.fold_left2 (fun a var label -> if var = attr then label::a else a) [] vars labels) in Printf.printf "Results for attr: %i\n" (attr);
      let result = ((float_of_int (List.length new_y)) /. (float_of_int (List.length labels))) *. (entropy new_y) in Printf.printf "Remainder for attr: %f\n\n" (result);
      (*Printf.printf "result: %f\n\n" (result);*)
      (remainder vars (attr - 1) labels) +. result;;

  (* Used for debugging so you can print out the values and labels lists.*)
  let print_list vals = List.fold_left (fun a value -> a ^ "," ^ (string_of_int value)) "" vals;;

  (* Calculates the remainder for each feature and then chooses the feature with the lowest remainder. Return: (winning featurue name * remainder) *)
  let choose_var x y attrs = 
    Feature_map.fold (fun key value a -> Printf.printf "Choose var (key) (value) (a) (y) (attr) : (%s) (%s) ((%s, %f)) (%s) (%i)\n" (key) (print_list value) (fst a) (snd a) (print_list y) (Feature_map.find key attrs);
                       let remainderVar = remainder value (Feature_map.find key attrs) y in
                       if remainderVar < (snd a) then (key, remainderVar) else a) x ("", 100000000.0)

  (* Take in a Map of features, a list of labels, and a map of the attributes for each feature.
     x_map = "feature name 1": [list of examples for feature]
            "feature name 2": [list of examples for feature] ...

     y = [list of labels]

     attr_map = "feature name 1": number of attributes (boolean = 1 for 0 and 1, 2 would be for feature with 3 attributes)
               "feature name 2": number of attributes*)
  let build_tree_v2 x_map y attr_map =

    (* partiion_x and partition_y return the new x or y where every example that lines up with the winning feature and attribute is returned.*)
    let partition_x (attr : int) win_x x = Feature_map.fold (fun key value_list a -> 
        Feature_map.add key (List.fold_right2 (fun value win_val a -> if win_val = attr then value::a else a) value_list win_x []) a) x Feature_map.empty in
    let partition_y attr x_val y = List.fold_right2 (fun val_x val_y a -> if Printf.printf "Partition y: (attr) (val_x) (val_y) : (%i) (%i) (%i)\n" (attr) (val_x) (val_y);
                                                      val_x = attr then val_y::a else a) x_val y [] in

    (* Used to build up the list of attributes to spit on. Example: 2 -> [0; 1; 2] *)
    let rec build_loop_list = function
      | -1 -> []
      | attr -> attr::(build_loop_list (attr - 1)) in

    (* Code for mode found here: https://rosettacode.org/wiki/Averages/Mode#OCaml *)
    let mode lst =
      let seen = Hashtbl.create 42 in
      List.iter (fun x ->
          let old = if Hashtbl.mem seen x then
              Hashtbl.find seen x
            else 0 in
          Hashtbl.replace seen x (old + 1))
        lst;
      let best = Hashtbl.fold (fun _ -> max) seen 0 in
      Hashtbl.fold (fun k v acc ->
          if v = best then k :: acc
          else acc)
        seen [] in

    (* Build the tree in a depth first traversal.*)
    let rec df_build x y attrs parent parent_attr depth =
      Printf.printf "Depth: %i\n" (depth);
      let classes = List.sort_uniq (fun x y -> if x > y then 1 else if x = y then -1 else 0) y in
      if (List.length classes) <= 0 then
        failwith "Cannot do anything with no features"
      else
        begin
          if (Feature_map.cardinal x) = 0 && List.length classes >= 1 then
            begin
              size := !size + 1;
              let v = G.V.create (string_of_int !size, string_of_int (List.hd classes)) in
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
              let v = G.V.create (string_of_int !size, (fst winner) ^ "<br/>" ^ "remainder: " ^ (string_of_float (snd winner))) in
              G.add_vertex g v;
              if !size != 1 then
                begin
                  let e = G.E.create parent parent_attr v in
                  G.add_edge_e g e;
                end;
              (* Go over each branch of the winning node and see what the next featurue should be, or if the next node is a leaf. *)
              List.iter (fun a -> let new_y = partition_y a winning_var_list y in let entropy_y = entropy new_y in Printf.printf "Entropy check: %f\n" (entropy_y);
                          Printf.printf "New y: [%s]\n" (print_list new_y);
                          if entropy_y <= 0.000001 && List.length new_y > 0 (* Leaf node because all labels are of 1 type.*)
                          then
                            begin
                              size := !size + 1; 
                              let leaf = G.V.create (string_of_int !size, string_of_int (List.hd new_y)) in 
                              G.add_vertex g leaf;
                              let leaf_edge = G.E.create v a leaf in
                              G.add_edge_e g leaf_edge
                            end 
                          else 
                            let entropy_old_y = entropy y in Printf.printf "Entropy check with old y: %f\n" (entropy_old_y);
                            Printf.printf "old y check: [%s]\n" (print_list y);
                            Printf.printf "New y: [%s]\n" (print_list new_y);
                            if List.length new_y <= 0 then (* Leaf node because partition will have no labels to look at.*)
                              begin
                                size := !size + 1; 
                                let majority = mode y in
                                let leaf = G.V.create (string_of_int !size, string_of_int (List.hd majority)) in 
                                G.add_vertex g leaf;
                                let leaf_edge = G.E.create v a leaf in
                                G.add_edge_e g leaf_edge
                              end 
                            else 
                              df_build (partition_x a winning_var_list remove_winner) new_y new_attrs v a (depth + 1)) attr_list 
            end
        end 
    in df_build x_map y attr_map (G.V.create ("-1", "should not exist")) 0 1;  Dot.output_graph stdout g
end

