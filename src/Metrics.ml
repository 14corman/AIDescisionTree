(* open DecisionTree *)

module Feature_map = Map.Make(String) ;;

let false_positive pred labels attr = List.fold_left2 (fun a p l -> if attr != l && l = p then a +. 1.0 else a) 0.0 pred labels
let true_positive pred labels attr  = List.fold_left2 (fun a p l -> if attr = l && l = p then a +. 1.0 else a) 0.0 pred labels
let false_negative pred labels attr  = List.fold_left2 (fun a p l -> if attr != l && l != p then a +. 1.0 else a) 0.0 pred labels

let rec f1_micro_average pred labels attr = 
  let tp = List.fold_left2 (fun a p_list l_list -> a +. true_positive p_list l_list attr) 0.0 pred labels in
  let fp = List.fold_left2 (fun a p_list l_list -> a +. false_positive p_list l_list attr) 0.0 pred labels in
  let fn = List.fold_left2 (fun a p_list l_list -> a +. false_negative p_list l_list attr) 0.0 pred labels in
  let (tp2, fp2, fn2) = f1_micro_average pred labels (attr - 1) in
  (tp +. tp2, fp +. fp2, fn +. fn2)

let f1_score pred labels attr =
  let (tp, fp, fn) = f1_micro_average pred labels attr in
  let precision tp fp = tp /. (tp +. fp) in
  let recall tp fn = tp /. (tp +. fn) in 
  let p = precision tp fp in
  let r = recall tp fn in
  2.0 *. (p *. r) /. (p +. r)

let shuffle d rand =
  let nd = List.map2 (fun r c -> (r, c)) rand d in
  let sond = List.sort compare nd in
  List.map snd sond

(* Code found here: https://stackoverflow.com/a/13278538 *)
let split xs size =
  let (_, r, rs) =
    (* fold over the list, keeping track of how many elements are still
       missing in the current list (csize), the current list (ys) and
       the result list (zss) *) 
    List.fold_left (fun (csize, ys, zss) elt ->
        (* if target size is 0, add the current list to the target list and
           start a new empty current list of target-size size *)
        if csize = 0 then (size - 1, [elt], zss @ [ys])
        (* otherwise decrement the target size and append the current element
           elt to the current list ys *)
        else (csize - 1, ys @ [elt], zss))
      (* start the accumulator with target-size=size, an empty current list and
         an empty target-list *)
      (size, [], []) xs
  in
  (* add the "left-overs" to the back of the target-list *)
  rs @ [r]
;;

(*
let build_cross_feature x num_examples list_maps name = 
  let lists = split x num_examples in
  if List.length list_maps = 0 then
    let maps = List.fold_left (fun a _ -> ((* DecisionTree.DTree. *) Feature_map.empty)::a) [] lists in


    let cross_validation k x y = 
      let rec build_loop_list = function
        | -1 -> []
        | k -> k::(build_loop_list (k - 1)) in
      let rec build_new_x num x = 
        let rec build_feature num list = 
          let num_per_fold =  (List.length y) / k in
          let random_list = List.fold_right (fun a label -> Random.bits ()::a) [] y in
          let k_list = build_loop_list k in
          let list_xs = List.fold_left (fun a _ -> ) [] k_list
*)
