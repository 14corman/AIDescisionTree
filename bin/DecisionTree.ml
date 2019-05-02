open Graph;;

module EDGE = struct
  type t = int
  let compare : t -> t -> int = Pervasives.compare
  let default = -1
end

module DTree = struct
  exception Var_attribute_mismatch
  module G = Imperative.Digraph.AbstractLabeled(struct type t = string * string end)(EDGE)
  module Display = struct
    include G
    (* https://b0-system.github.io/odig/doc/ocamlgraph/Graph/Graphviz/DotAttributes/index.html *)
    let vertex_name v =  fst (V.label v)
    let edge_name = E.label
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_attributes v = [`Shape `Box; `Style `Filled; `Style `Rounded; `HtmlLabel (snd (V.label v))]
    let default_edge_attributes _ = []
    let edge_attributes e = [`Label (string_of_int (E.label e))]
    let get_subgraph _ = None
  end
  module DotOutput = Graphviz.Dot(Display)
  module B = Builder.I(G)
  module DotInput = 
    Dot.Parse
      (B)
      (struct 
        let get_label attr_list = List.fold_left (fun _ attr -> 
            List.fold_left (fun a ((id: Dot_ast.id), (value: Dot_ast.id option)) -> 
                let id_string = match id with
                  | Dot_ast.Ident s
                  | Dot_ast.Number s
                  | Dot_ast.String s
                  | Dot_ast.Html s -> s 
                in if id_string = "label" then match value with
                  | None -> "None"
                  | Some (Dot_ast.Ident s)
                  | Some (Dot_ast.Number s)
                  | Some (Dot_ast.String s)
                  | Some (Dot_ast.Html s) -> s
                else a) "" attr) "" attr_list

        let node (id,_) (attr_list: Dot_ast.attr list) = match id with
          | Dot_ast.Ident s
          | Dot_ast.Number s
          | Dot_ast.String s
          | Dot_ast.Html s -> (s, get_label attr_list)

        let edge (attr_list: Dot_ast.attr list) = int_of_string (get_label attr_list)
      end)
  module Feature_map = Map.Make(String)

  (* Will write the graph out to a dot file with the given name. *)
  let write_dot_to_file graph file_name = 
    let file = open_out_bin file_name in
    DotOutput.output_graph file graph

  (* Will take the file name of a precomputed graph dot file and parse it to build a graph that will be returned. *)
  let parse_dot_file file_name = DotInput.parse file_name

  let g = G.create ()

  (* The number of vertices in the graph. Will be used to make vertex ids later.*)
  let size = ref 0

  (* Used to print the graph out to standard out.*)
  let print_graph g = DotOutput.output_graph stdout g


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
      seen []

  (* Calculates the entropy of a given list of labels.*)
  let entropy ys = 
    let log2 = 1.0 /. log 2.0 in
    let (p, n) = List.partition  (fun y -> y = 1) ys in
    (*Printf.printf "p: %i, n: %i\n" (List.length p) (List.length n);*)
    let num_p = float_of_int (List.length p) in
    let num_n = float_of_int (List.length n) in
    let combined = num_p +. num_n +. 0.0000000000000001 in
    let p_combined = num_p /. combined +. 0.0000000000000001 in 
    (*Printf.printf "Positive division: %f\n" (p_combined);*)
    let n_combined = num_n /. combined +. 0.0000000000000001 in 
    (*Printf.printf "Negative division: %f\n" (n_combined);*)
    let entropy_val = (0.0 -. p_combined) *. ((log p_combined) *. log2) -. n_combined *. ((log n_combined) *. log2) in
    (*Printf.printf "Entropy value: %f\n" (entropy_val);*)
    entropy_val

  (* Need to change label = 1 to something else at some point if we need to do multiclass classification.
     Computes the remainder of labels and attributes. *)
  let rec remainder vars attr labels =
    if attr = -1 then 0.0 else 
      let new_y = (List.fold_left2 (fun a var label -> if var = attr then label::a else a) [] vars labels) in
      (*Printf.printf "Results for attr: %i\n" (attr);*)
      let result = ((float_of_int (List.length new_y)) /. (float_of_int (List.length labels))) *. (entropy new_y) in 
      (*Printf.printf "Remainder for attr: %f\n\n" (result);*)
      (*Printf.printf "result: %f\n\n" (result);*)
      (remainder vars (attr - 1) labels) +. result

  (* Used for debugging so you can print out the values and labels lists.*)
  let string_of_list vals = List.fold_left (fun a value -> a ^ "," ^ (string_of_int value)) "" vals

  let string_of_string_map map = (Feature_map.fold (fun _ value string -> string ^ value ^ ",") map "[") ^ "]"
  let str_of_str_map_w_key map = (Feature_map.fold (fun key value string -> string ^ key ^ ":" ^ value ^ ",") map "[") ^ "]"
  let string_of_int_map map = (Feature_map.fold (fun _ value string -> string ^ string_of_int value ^ ",") map "[") ^ "]"

  (* Calculates the remainder for each feature and then chooses the feature with the lowest remainder. Return: (winning featurue name * remainder) *)
  let choose_var x y attrs = 
    Feature_map.fold (fun key value a -> 
        (*Printf.printf "Choose var (key) (value) (a) (y) (attr) : (%s) (%s) ((%s, %f)) (%s) (%i)\n" (key) (string_of_list value) (fst a) (snd a) (string_of_list y) (Feature_map.find key attrs);*)
        let remainderVar = remainder value (Feature_map.find key attrs) y in
        if remainderVar < (snd a) then (key, remainderVar) else a) x ("", 100000000.0)

  (* Take in a Map of features, a list of labels, and a map of the attributes for each feature.
     x_map = "feature name 1": [list of examples for feature]
            "feature name 2": [list of examples for feature] ...

     y = [list of labels]

     attr_map = "feature name 1": number of attributes (boolean = 1 for 0 and 1, 2 would be for feature with 3 attributes)
               "feature name 2": number of attributes*)
  let build_tree x_map y attr_map depth =
    (*let num_classes = List.sort_uniq (fun x y -> if x > y then 1 else if x = y then -1 else 0) y in*)

    (* partiion_x and partition_y return the new x or y where every example that lines up with the winning feature and attribute is returned.*)
    let partition_x (attr : int) win_x x = Feature_map.fold (fun key value_list a -> 
        Feature_map.add key (List.fold_right2 (fun value win_val a -> if win_val = attr then value::a else a) value_list win_x []) a) x Feature_map.empty in
    let partition_y attr x_val y = List.fold_right2 (fun val_x val_y a -> if 
                                                      (*Printf.printf "Partition y: (attr) (val_x) (val_y) : (%i) (%i) (%i)\n" (attr) (val_x) (val_y);*)
                                                      val_x = attr then val_y::a else a) x_val y [] in

    (* Used to build up the list of attributes to spit on. Example: 2 -> [0; 1; 2] *)
    let rec build_loop_list = function
      | -1 -> []
      | attr -> attr::(build_loop_list (attr - 1)) in

    (* Get a mapf all the classes and their number of outcomes. *)
    let compute_num_samples (list : int list) = 
      List.fold_left (fun map label -> if (Feature_map.mem (string_of_int label) map) then 
                         Feature_map.add (string_of_int label) ((Feature_map.find (string_of_int label) map) + 1) (Feature_map.remove (string_of_int label) map)
                       else
                         Feature_map.add (string_of_int label) 1 map) Feature_map.empty list in

    (* Build the tree in a depth first traversal.*)
    let rec df_build x y attrs parent parent_attr depth max_depth =
      Printf.printf "Depth: %i    Max_depth: %i\n" (depth) (max_depth);
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
              (*Printf.printf "Winner = %s" (fst winner); Printf.printf ": %f\n" (snd winner);*)
              let attr_list = build_loop_list (Feature_map.find (fst winner) attrs) in
              let new_attrs = Feature_map.remove (fst winner) attrs in
              let winning_var_list = Feature_map.find (fst winner) x in
              let remove_winner = Feature_map.remove (fst winner) x in
              size := !size + 1;
              let v = G.V.create (string_of_int !size, (fst winner) ^ "<br/>" ^ "remainder: " ^ (string_of_float (snd winner)) ^ "<br/>Samples:" ^ (string_of_int_map (compute_num_samples y))) in
              G.add_vertex g v;
              (* Go over each branch of the winning node and see what the next featurue should be, or if the next node is a leaf. *)
              List.iter (fun a -> let new_y = partition_y a winning_var_list y in let entropy_y = entropy new_y in 
                          (*Printf.printf "Entropy check: %f\n" (entropy_y);*)
                          (*Printf.printf "New y: [%s]\n" (string_of_list new_y);*)
                          if (entropy_y <= 0.000001 && List.length new_y > 0) (* Leaf node because all labels are of 1 type.*)
                          then
                            begin
                              size := !size + 1; 
                              let leaf = G.V.create (string_of_int !size, string_of_int (List.hd new_y) ^ "<br/>Samples:" ^ (string_of_int_map (compute_num_samples new_y))) in 
                              G.add_vertex g leaf;
                              let leaf_edge = G.E.create v a leaf in
                              G.add_edge_e g leaf_edge
                            end 
                          else 
                            (*let entropy_old_y = entropy y in 
                              Printf.printf "Entropy check with old y: %f\n" (entropy_old_y);*)
                            (*Printf.printf "old y check: [%s]\n" (string_of_list y);*)
                            (*Printf.printf "New y: [%s]\n" (string_of_list new_y);*)

                            (* Leaf node because partition will have no labels to look at or distributions between parent and child will be the same.*)
                          if List.length new_y <= 0 || entropy_y = (snd winner) then 
                            begin
                              size := !size + 1; 
                              let majority = mode y in
                              let leaf = G.V.create (string_of_int !size, string_of_int (List.hd majority) ^ "<br/>Samples:" ^ (string_of_int_map (compute_num_samples y))) in 
                              G.add_vertex g leaf;
                              let leaf_edge = G.E.create v a leaf in
                              G.add_edge_e g leaf_edge
                            end 
                          else

                            (* The depth of the node is at the max alloweable depth, so it is forced to be a leaf.*)
                          if depth + 1 = max_depth then 
                            begin
                              size := !size + 1; 
                              let majority = mode new_y in
                              let leaf = G.V.create (string_of_int !size, string_of_int (List.hd majority) ^ "<br/>Samples:" ^ (string_of_int_map (compute_num_samples new_y))) in 
                              G.add_vertex g leaf;
                              let leaf_edge = G.E.create v a leaf in
                              G.add_edge_e g leaf_edge
                            end 
                          else 
                            df_build (partition_x a winning_var_list remove_winner) new_y new_attrs v a (depth + 1) max_depth) attr_list;
              if fst (G.V.label parent) != "-1" then
                begin
                  let successors = G.succ g v in
                  let successors_uniq = List.sort_uniq (fun x y -> Pervasives.compare x y) (List.fold_left (fun a vertex -> (snd (G.V.label vertex))::a) [] successors) in
                  (* This node only has leaf nodes as children, and all those leaf nodes have the same value. *)
                  if List.length successors_uniq = 1 then
                    begin
                      let old_size = fst (G.V.label v) in
                      G.remove_vertex g v;
                      List.iter (fun vertex -> G.remove_vertex g vertex) successors;
                      let new_v = G.V.create (old_size, List.hd successors_uniq) in
                      G.add_vertex g new_v;
                      let e = G.E.create parent parent_attr new_v in
                      G.add_edge_e g e;
                    end 
                  else
                    let e = G.E.create parent parent_attr v in
                    G.add_edge_e g e;
                end; 
            end
        end 
    in 
    let dumby_vertex = G.V.create ("-1", "should not exist") in
    match depth with
    | None -> df_build x_map y attr_map dumby_vertex 0 1 max_int; G.remove_vertex g dumby_vertex; g
    | Some md -> df_build x_map y attr_map dumby_vertex 0 1 md; G.remove_vertex g dumby_vertex; g
end;;

