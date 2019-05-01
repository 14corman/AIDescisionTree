(* Methods to calculate parts for f1, accuracy, and confusion matrix.*)
let false_positive pred labels attr = List.fold_left2 (fun a p l -> if attr = l && l != p then a +. 1.0 else a) 0.0 pred labels
let true_positive pred labels attr  = List.fold_left2 (fun a p l -> if attr = l && l = p then a +. 1.0 else a) 0.0 pred labels
let false_negative pred labels attr  = List.fold_left2 (fun a p l -> if attr != l && l != p then a +. 1.0 else a) 0.0 pred labels
let true_negative pred labels attr  = List.fold_left2 (fun a p l -> if attr != l && l = p then a +. 1.0 else a) 0.0 pred labels

(* Calculate the values for micro sums to be able to do micro averages for best metrics. *)
let rec micro_sum pred labels attr = if attr = -1 then (0.0, 0.0, 0.0, 0.0) else
    let tp = List.fold_left2 (fun a p_list l_list -> a +. true_positive p_list l_list attr) 0.0 pred labels in
    let fp = List.fold_left2 (fun a p_list l_list -> a +. false_positive p_list l_list attr) 0.0 pred labels in
    let fn = List.fold_left2 (fun a p_list l_list -> a +. false_negative p_list l_list attr) 0.0 pred labels in
    let tn = List.fold_left2 (fun a p_list l_list -> a +. true_negative p_list l_list attr) 0.0 pred labels in
    let (tp2, fp2, fn2, tn2) = micro_sum pred labels (attr - 1) in
    (tp +. tp2, fp +. fp2, fn +. fn2, tn +. tn2)

(* Print the confusion matrix for 1 fold. *)
let rec print_confusion_matrix pred labels attr =
  if attr != -1 then
    let tp = true_positive pred labels attr in
    let fp = false_positive pred labels attr in
    let fn = false_negative pred labels attr in
    let tn = true_negative pred labels attr in
    Printf.printf "Matrix for attr: %i\n" (attr);
    Printf.printf "        Positive | Negative \n";
    Printf.printf "True  | %f | %f\n" (tp) (tn);
    Printf.printf "-------------------\n";
    Printf.printf "False | %f | %f\n\n" (fp) (fn);
    print_confusion_matrix pred labels  (attr - 1)

(* Calculate the accuracy for all of cross validation. Pred and labels are lists of cross validation labels and predictions. *)
let accuracy pred labels attr = 
  let (tp, fp, fn, tn) = micro_sum pred labels attr in
  (tp +. fp) /. (tn +. tp +. fp +. fn)

(* Calculate the micro-average F1 score. Pred and labels are lists of cross validation labels and predictions.*)
let f1_score pred labels attr =
  let (tp, fp, fn, _) = micro_sum pred labels attr in
  let precision tp fp = tp /. (tp +. fp) in
  let recall tp fn = tp /. (tp +. fn) in 
  let p = precision tp fp in
  let r = recall tp fn in
  (*Printf.printf "Precision: %f\n" (p);
    Printf.printf "Recall: %f\n" (r);*)
  2.0 *. (p *. r) /. (p +. r)