open DecisionTree ;;
open TreeClassifier ;;
open Parser ;;
open Metrics ;;
(* open Printf ;; *)

(* Shuffle a list using the input seed.
   The same seed will shuffle lists of equal length the same way
*)
let fisher_yates (seed : int) (hat : 'a list) =
  (* Remove the nth element from takeFrom and return the remainder *)
  let rec getNth takeFrom n =
    match takeFrom with
    | [] -> failwith "List index out of bounds"
    | (x :: []) -> if n = 0 then (x, []) else failwith "List index out of bounds"
    | (x :: xs) -> (
        if n = 0 then
          (x, xs)
        else
          let (removed, bucket) = getNth xs (n - 1) in
          (removed, x :: bucket)
      )
  in
  (* One at a time, take a random item out of takeFrom and add it to prependTo *)
  let rec helper takeFrom prependTo length =
    match takeFrom with
    | (_ :: _ :: _) -> (
        let (removed, bucket) = getNth takeFrom (Random.int length) in
        helper bucket (removed :: prependTo) (length - 1)
      )
    | _ -> takeFrom @ prependTo
  in
  (* Initialize with the given seed *)
  Random.init seed ;
  helper hat [] (List.length hat)
;;

(* Split a list at the given index

   Returns (head, tail)
*)
let split_list indx l =
  let curIndx = ref ~-1 in
  List.partition (fun _ -> curIndx := !curIndx + 1 ; !curIndx < indx ) l

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


let shuffle_and_partition (k : int) (feature_map : int list DTree.Feature_map.t) (labels : int list) =
  Random.self_init () ;
  let seed = Random.bits () in
  let feat_map = DTree.Feature_map.map (fisher_yates seed) feature_map in
  let labs = fisher_yates seed labels in
  (* let (feat_map, labs) = shuffle features feature_map labels in *)
  (partition_map k feat_map, partition k labs [])
;;

(* Given a list of lists, combine all of them except for the excludeth.
   Returns the excluded list and the combined lists *)
let rec combine_list_partitions l (exclude : int) =
  match l with
  | [] -> ([],[])
  | (part :: parts) -> (
      if exclude = 0 then
        (part, List.concat parts)
      else
        let (excluded, combined) = combine_list_partitions parts (exclude-1) in
        (excluded, List.append part combined)
    )
;;

(* Returns two maps. One's values are the excluded lists from combine_line_partitions.
   The other's values are the combined lists from combine_list_partitions. *)
let combine_map_partitions m (exclude : int) =
  DTree.Feature_map.fold (fun f ps (ex, com) ->
      let (excluded, combined) = combine_list_partitions ps exclude in
      (DTree.Feature_map.add f excluded ex, DTree.Feature_map.add f combined com))
    m (DTree.Feature_map.empty, DTree.Feature_map.empty)
;;

(* Builds a tree using all but the partitioneth list in the partitioned dataset. 
    Classifies the remaining list using the built tree *)
let run_partition partitioned_map partitioned_labels feat_val_counts depth partition =
  let (excluded_examples, combined_examples) = combine_map_partitions partitioned_map partition in
  let (excluded_labels, combined_labels) = combine_list_partitions partitioned_labels partition in
  let g = build_tree combined_examples combined_labels feat_val_counts depth in
  (*print_graph g;*)
  let predictions = classify_examples g excluded_examples in
  let incorrect = List.fold_left2 (fun err pred act -> if pred = act then err else err + 1)
                                  0 (classify_examples g combined_examples) combined_labels in
  (* Printf.printf "pred: [" ; List.iter (Printf.printf "%d; ") predictions ; Printf.printf "]\n" ;
     Printf.printf "act:  [" ; List.iter (Printf.printf "%d; ") excluded_labels ; Printf.printf "]\n\n" ; *)
  predictions, excluded_labels, (float_of_int incorrect) /. (float_of_int (List.length combined_labels))
;;


(* Cross validate  *)
(* Problem -- num_feature_values isn't guaranteed to be correct for a given partitioning *)

(* Returns a graph covering all of the examples, a list of lists of predicted labels,
     a list of lists of actual labels, and the training error for this partition *)
let cross_validator (k : int) (d : int option) (feature_map,labels,num_feature_values) =
  (* let _ = DTree.Feature_map.fold (fun feat _ so_far -> feat :: so_far) feature_map [] in  (* first _ => features *) *)
  if List.length labels < k then
    failwith "The number of folds should not exceed the number of examples"
  else
    let (partitioned_map, partitioned_labels) = shuffle_and_partition k feature_map labels in

    (* Gather the predicted labels for each partition, the actual labels, and the sum of the training errors *)
    let (predicted, actual, t_err) = List.fold_left (fun (pred, act, err) part ->
        let (p, a, tr_err) = run_partition partitioned_map partitioned_labels num_feature_values d part in
        (p :: pred, a :: act, tr_err +. err))
        ([],[],0.0)
        (range 0 k)
    in

    let g = build_tree feature_map labels num_feature_values d in
    (g, predicted, actual, t_err /. (float_of_int k))
;;

(* Returns the decision tree for the entire dataset,
   a list of lists of predicted class labels, a list of lists of class labels,
   and the average training error. *)
let cross_validate (k : int) (d : int option) (data_file : string) =
  cross_validator k d (parse data_file)
;;

(*
(* k set to 4 *)
let select_model (data_file : string) (low : int option) (high : int option) =
  let ds = parse data_file in
  let min_depth =
    match low with
    | None -> 0
    | Some c -> c
  in
  let max_depth =
    match high with
    | None -> No
  in

  List.fold_left (fun d ->
    let (predicted, actual, t_err) = cross_validator 4 d ds in
  )
  ([],[])
  (range 1 11)
  None
;; *)
