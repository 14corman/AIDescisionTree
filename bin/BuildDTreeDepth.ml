open TreeClassifier;;
open Metrics;;
open CrossValidate;;

(* command line *)
let f_ = ref "dumby"
let dir_ = ref None
let d_ = ref None

let arg_spec =
  ["-f", Arg.String (fun i -> f_ := i),
   " <string>  File name of csv file to use. Does not include .csv.";
   "-dir", Arg.String (fun i -> dir_ := Some i),
   " <string>  Optional string to specify directory of csv file if not in current directory.";
   "-m", Arg.Int (fun _ -> ()),
   " <int>  A dumby input that satisfies part of the assignment.";
   "-d", Arg.Int (fun i -> d_ := Some i),
   " <int>  The maximum depth to make the decision tree."
  ]
let () = Arg.parse arg_spec (fun _ -> ()) "usage: build tree <options>"

let dir = match !dir_ with
  | None -> "" 
  | Some d -> d ^ "/"

let depth = match !d_ with
  | None -> failwith "Max depth of tree not given"
  | Some d -> Some d

let name = !f_
let f = dir ^ !f_ ^ ".csv"
let output_file = dir ^ !f_ ^ ".dot"
let () = Printf.printf "Working on dataset: %s\n" (name);;

let find_max list = List.fold_left (fun a ls -> let new_a = List.fold_left (fun a l -> if l > a then l else a) 0 ls in if new_a > a then new_a else a) 0 list;;

let (g, predictions, labels, training_error) = select_model (Some 1) depth f;;
let () = print_graph g;;
let () = write_dot_to_file g output_file;;
let max_label = find_max labels;;
let f1 = f1_score predictions labels max_label;;
let () = print_confusion_matrices predictions labels max_label;;
let () = Printf.printf "\nF1 score: %f\n" (f1);;
