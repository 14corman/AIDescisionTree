open DecisionTree
include DTree

(* See if string 1 contains string 2. *)
let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false

(* Split the label on the "<br/>" sequence. This will return a list. *)
let split = Str.split (Str.regexp "<br/>")

(* Gets a label of a node and returns the name of the feature from that label. *)
let get_name_from_label l = List.hd (split l)

(* Creates a conversion map from feature names to node ids. This will be used to convert the given map and then passed to dfs to classify. *)
let get_map_conversion g = G.fold_vertex (fun vertex a -> if contains (snd (G.V.label vertex)) "<br/>"
                                           then Feature_map.add (get_name_from_label (snd (G.V.label vertex))) (fst (G.V.label vertex)) a else a) g Feature_map.empty

(* Find the root of graph which will be a tree. *)
let find_root g = G.fold_vertex (fun vertex a -> if fst (G.V.label vertex) = "1" then Some vertex else a) g None

(* Do a breadth first search of the graph to find the correct leaf node and subsequent prediction and return that. *)
let rec dfs g vertex x = 
  let edges = G.succ_e g vertex in
  if List.length edges = 0 then
    (snd (G.V.label vertex))
  else
    let edge_label = Feature_map.find (fst (G.V.label vertex)) x in
    let next_vertex = List.fold_left (fun a edge -> if G.E.label edge = edge_label then G.E.dst edge else a) vertex edges in
    dfs g next_vertex x

(* Classifiy a single example. This x map will have the following structure:
    feat1: value
    feat2: value
    ... *)
let classify_example g x = match find_root g with
  | None -> failwith "Tree does not have root"
  | Some root -> 
    let conversion = get_map_conversion g in
    let new_x = Feature_map.fold (fun key value a -> match Feature_map.find_opt key conversion with
        | None -> a
        | Some label -> Feature_map.add label value a) x Feature_map.empty in
    int_of_string  (List.hd (split (dfs g root new_x)))

(* Classifify a map list of examples. This will have the same structure as DTree.build_tree. It will return a list of classifications. *)
let classify_examples g xs = 
  let take_next_examples x = Feature_map.fold (fun key value a -> Feature_map.add key (List.tl value) a) x Feature_map.empty in
  let rec compute_example g remain_x num_examples = if num_examples = 0 then [] else
      let example = Feature_map.fold (fun key value a -> Feature_map.add key (List.hd value) a) remain_x Feature_map.empty in
      (classify_example g example)::(compute_example g (take_next_examples remain_x) (num_examples - 1)) in
  let total_examples = List.length (snd (List.hd (Feature_map.bindings xs))) in
  compute_example g xs total_examples


