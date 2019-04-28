open TreeClassifier ;;
open Parser ;;

(* let g = TreeClassifier.build_tree feat_map labels domain *)

let Feature_map = Map.Make(String) ;;

let rec shuffle features feature_map labels examples =
  let rec shuffler l =
    match l with
    | [] -> []
    | x :: [] -> l
    | xs ->
      let (front, back) = List.partition (fun _ -> Random.bool()) l in
      List.rev_append (shuffler front) (shuffler back)
  in
  match feature_map, labels with
  | [], [] -> examples
  | (feat :: xs), (label :: ys) -> shuffler (List.fold_right (fun value accum -> value :: accum) features [label])
  | a, b -> failwith "Failed to match examples with their class labels in shuffle"
;;

let rec split_list l indx =
  if indx < 1 then
    

let rec partition (k : int) examples partitions =
  if k < 1 then
    
  else
    partition (k-1)

let shuffle_and_partition k features feature_map labels = partition k (shuffle features feature_map labels []) [] ;;

let cross_validate (k : int) (data_file : string) =
  let (feature_map, labels, num_feature_values) = Parser.parse data_file in
  let features = Feature_map.fold (fun feat so_far -> feat :: so_far) feature_map [] in
  if List.length labels < k then
    failwith "The number of folds should not exceed the number of examples"
  else
    let partitions = shuffle_and_partition k features feature_map labels in


