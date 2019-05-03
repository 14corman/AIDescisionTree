open CrossValidate ;;
(* open Parser ;;
open TreeClassifier ;; *)

(*let (_,_,_,err) = cross_validate 10 None "data/breast-cancer.csv" ;;
Printf.printf "%f\n" err ;;"data/house-votes-84.csv"*)
let () = select_model (Some 1) (Some 10) "data/house-votes-84.csv" ;;

(*
let (feature_map, labels, feature_values) = parse "data/house-votes-84.csv" ;;
let g = build_tree feature_map labels feature_values (Some 1) ;;
let () = print_graph g ;; *)
