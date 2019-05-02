open TreeClassifier;;
open Parser;;
open Metrics;;
(* open CrossValidate ;; *)


let (feature_map, labels, value_map) = parse "data/house-votes-84.csv" ;;(*Sys.argv.(1) *);;
(* f1 = cross_validation feature_map labels value_map*);;
let g = build_tree feature_map labels value_map None;;
let () = print_graph g;;
let predictions = classify_examples g feature_map;;
let f1 = f1_score [predictions] [labels] 1;;
let () = print_confusion_matrix predictions labels 1;;
let () = Printf.printf "F1 score: %f\n" (f1);;
