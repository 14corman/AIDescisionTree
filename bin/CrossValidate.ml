open DecisionTree ;;
open Parser

(* module Feature_map = Map.Make(String) ;; *)
(* let g = TreeClassifier.build_tree feat_map labels domain *)

(* Takes a list of the names of features, the Feature_map containing the dataset, and
     a list of the examples' class labels

   Returns a Feature_map containing the same input examples, but the values are all shuffled
     in the same way, a list of the examples' labels shuffled the same was as the Feature_map vals
*)
let rec shuffle features (feature_map : int list DTree.Feature_map.t) (labels : int list) =
  let rec shuffler k feature_m labels =
    if k < 1 then
      (feature_m, labels)
    else
      (* A list of bools, true if the element at that index goes into the left partition, false otherwise *)
      let parts = List.fold_right (fun _ acc -> Random.bool() :: acc) labels [] in
      (* A feature map with all of the values shuffled according to parts *)
      let feat_map = DTree.Feature_map.map (fun val_list -> let (tail, head) = List.fold_right2 (fun v b (h, t) -> if b then (v :: h, t) else (h, v :: t)) val_list parts ([],[]) in
                                             List.rev_append head tail) feature_m in
      shuffler (k-1) feat_map (let (tail, head) = List.fold_right2 (fun v b (h, t) -> if b then (v :: h, t) else (h, v :: t)) labels parts ([],[]) in List.rev_append head tail)
  in
  shuffler 10 feature_map labels 
;;

(* Split a list at the given index

   Returns (head, tail)
*)
let rec split_list indx l =
  let curIndx = ref ~-1 in
  List.partition (fun el -> curIndx := !curIndx + 1 ; !curIndx < indx ) l

(* Partition a list into k lists

   Returns a list of k lists (elements in the reverse order of the input list)
*)
let rec partition (k : int) examples partitions =
  if k < 2 then
    examples :: partitions
  else
    let (part, exes) = split_list (List.length examples / k) examples in
    partition (k-1) exes (part :: partitions)
;;

(* Partitions all of the values in a map into lists of k lists *)
let partition_map (k : int) (feature_map : int list DTree.Feature_map.t) =
  DTree.Feature_map.map (fun v-> partition k v []) feature_map
;;


let shuffle_and_partition (k : int) (features : string list) (feature_map : int list DTree.Feature_map.t) (labels : int list) = 
  let (feat_map, labs) = shuffle features feature_map labels in
  (partition_map k feat_map, partition k labs [])
;;

(*
let cross_validate (k : int) (d : depth) (data_file : string) =
  let (feature_map, labels, num_feature_values) = parse data_file in
  let features = DTree.Feature_map.fold (fun feat l so_far -> feat :: so_far) feature_map [] in
  if List.length labels < k then
    failwith "The number of folds should not exceed the number of examples"
  else
    let partitions = shuffle_and_partition k features feature_map labels in
    parititions
;; *)
