open TreeClassifier;;
open Parser;;
open Metrics;;

let (feature_map, labels, value_map) = parse "data/house-votes-84.csv" ;;(*Sys.argv.(1) *);;
let () = Printf.printf "values from map: %s\n" (string_of_int_map value_map);;
(* f1 = cross_validation feature_map labels value_map;;*)
let g = build_tree feature_map labels value_map;;
let () = print_graph g;;
let predictions = classify_examples g feature_map;;
let f1 = f1_score [predictions] [labels] 1;;
let () = print_confusion_matrix predictions labels 1;;
let () = Printf.printf "F1 score: %f\n" (f1);;
let () = Printf.printf "Labels: %s\n" (string_of_list labels);;
let () = Printf.printf "Predictions: %s\n" (string_of_list predictions);;
