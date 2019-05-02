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
  let predictions = classify_examples g excluded_examples in
  (* Printf.printf "pred: [" ; List.iter (Printf.printf "%d; ") predictions ; Printf.printf "]\n" ;
  Printf.printf "act:  [" ; List.iter (Printf.printf "%d; ") excluded_labels ; Printf.printf "]\n\n" ; *)
  predictions, excluded_labels
;;


(* Cross validate  *)
(* Problem -- num_feature_values isn't guaranteed to be correct for a given partitioning *)

(* Returns a graph covering all of the examples, a list of lists of predicted labels,
     and a list of lists of actual labels *)
let cross_validate (k : int) (d : int option) (data_file : string) =
  let (feature_map, labels, num_feature_values) = parse data_file in
  let _ = DTree.Feature_map.fold (fun feat _ so_far -> feat :: so_far) feature_map [] in  (* first _ => features *)
  if List.length labels < k then
    failwith "The number of folds should not exceed the number of examples"
  else
    let (partitioned_map, partitioned_labels) = shuffle_and_partition k feature_map labels in
    let (predicted, actual) = List.fold_left (fun (pred, act) part ->
                                                    let (p, a) = run_partition partitioned_map partitioned_labels num_feature_values d part in
                                                    (p :: pred, a :: act))
                                ([],[])
                                (range 0 k)
    in
    let g = build_tree feature_map labels num_feature_values in
    (g, predicted, actual)
;;
