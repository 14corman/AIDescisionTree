open DecisionTree ;;
open Parser

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
    | (x :: y :: xs) -> (
      let (removed, bucket) = getNth takeFrom (Random.int length) in
      helper bucket (removed :: prependTo) (length - 1)
    )
    | a -> takeFrom @ prependTo
  in
    (* Initialize with the given seed *)
    Random.init seed ;
    helper hat [] (List.length hat)
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


let shuffle_and_partition (k : int) (feature_map : int list DTree.Feature_map.t) (labels : int list) =
  Random.self_init () ;
  let seed = Random.bits () in
  let feat_map = DTree.Feature_map.map (fisher_yates seed) feature_map in
  let labs = fisher_yates seed labels in
  (* let (feat_map, labs) = shuffle features feature_map labels in *)
  (partition_map k feat_map, partition k labs [])
;;

(*
let cross_validate (k : int) (d : depth) (data_file : string) =
  let (feature_map, labels, num_feature_values) = parse data_file in
  let features = DTree.Feature_map.fold (fun feat l so_far -> feat :: so_far) feature_map [] in
  if List.length labels < k then
    failwith "The number of folds should not exceed the number of examples"
  else
    let partitions = shuffle_and_partition k feature_map labels in
    parititions
;;
*)