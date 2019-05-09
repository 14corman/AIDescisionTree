(** 
   Takes in a dot file that is a decision tree and a csv file that is a dataset. It then gets predicitions for
   all of the examples from the csv file using the decision tree. It prints out every prediciton and the actual
   class of that prediction. It then prints out the F1 score calculated from those predicitons and labels.
*)

open TreeClassifier;;
open Parser;;
open Metrics;;

(* command line *)
let f_ = ref "dumby"
let dir_ = ref None
let dot_ = ref "dumby"

let arg_spec =
  ["-f", Arg.String (fun i -> f_ := i),
   " <string>  File name of csv file to use. Does not include .csv.";
   "-dir", Arg.String (fun i -> dir_ := Some i),
   " <string>  Optional string to specify directory of csv file if not in current directory.";
   "-d", Arg.String (fun i -> dot_ := i),
   " <string>  The name of the dot file holding the tree. Does not include .dot."
  ]
let () = Arg.parse arg_spec (fun _ -> ()) "usage: classify tree <options>"

let dir = match !dir_ with
  | None -> "" 
  | Some d -> d ^ "/"

let name = !f_
let f = dir ^ !f_ ^ ".csv"
let input_tree = dir ^ !dot_ ^ ".dot"
let () = Printf.printf "Working on dataset: %s\n\n" (name);;

let find_max list = List.fold_left (fun a l -> if l > a then l else a) 0 list;;

let (feature_map, labels, value_map) = parse f (*"data/breast-cancer.csv"*) ;;
let g = parse_dot_file input_tree;;
let max_label = find_max labels;;
let predictions = classify_examples g feature_map;;
let () = List.iter2 (fun p a -> Printf.printf "Prediction: %i     Actual: %i\n" (p) (a)) predictions labels;;
let f1 = f1_score [predictions] [labels] max_label;;
let () = print_confusion_matrix predictions labels max_label;;
let () = Printf.printf "\nF1 score: %f\n" (f1);;
