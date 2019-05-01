open TreeClassifier;;
open Parser;;

let (feature_map, labels, value_map) = parse "C:\\OCaml64\\home\\CoryEdwards\\AIDescisionTree\\datahouse-votes-84.csv" ;;(*Sys.argv.(1) *);;
let g = build_tree feature_map labels value_map;;
let () = print_graph g;;