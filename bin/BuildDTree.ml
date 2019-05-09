(** 
   Builds a tree of maximum depth given a data file.
*)

open TreeClassifier;;
open Parser;;
open Metrics;;

(* command line *)
let f_ = ref "dumby"
let dir_ = ref None

let arg_spec =
  ["-f", Arg.String (fun i -> f_ := i),
   " <string>  File name of csv file to use. Does not include .csv.";
   "-dir", Arg.String (fun i -> dir_ := Some i),
   " <string>  Optional string to specify directory of csv file if not in current directory.";
   "-m", Arg.Int (fun _ -> ()),
   " <int>  A dumby input that satisfies part of the assignment."
  ]
let () = Arg.parse arg_spec (fun _ -> ()) "usage: build tree <options>"

let dir = match !dir_ with
  | None -> "" 
  | Some d -> d ^ "/"

let name = !f_
let f = dir ^ !f_ ^ ".csv"
let output_file = dir ^ !f_ ^ ".dot"
let () = Printf.printf "Working on dataset: %s\n" (name);;

let find_max list = List.fold_left (fun a l -> if l > a then l else a) 0 list;;

let (feature_map, labels, value_map) = parse f (*"data/breast-cancer.csv"*) ;;
let g = build_tree feature_map labels value_map None;;
let () = print_graph g;;
let () = write_dot_to_file g output_file;;
let max_label = find_max labels;;
let predictions = classify_examples g feature_map;;
let f1 = f1_score [predictions] [labels] max_label;;
let () = print_confusion_matrix predictions labels max_label;;
let () = Printf.printf "\nF1 score: %f\n" (f1);;
