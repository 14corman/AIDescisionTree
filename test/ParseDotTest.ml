open TreeClassifier

(* Parse the tree from the file. *)
let g = TreeClassifier.parse_dot_file "test.dot"
let () = TreeClassifier.print_graph g

let example = TreeClassifier.Feature_map.empty
let example = TreeClassifier.Feature_map.add "handicaped-infants"          1 example
let example = TreeClassifier.Feature_map.add "water-project-cost"          1 example
let example = TreeClassifier.Feature_map.add "adoption-of-the-budget"      1 example
let example = TreeClassifier.Feature_map.add "physician-fee-freeze"        0 example
let example = TreeClassifier.Feature_map.add "el-salvador-aid"             0 example
let example = TreeClassifier.Feature_map.add "religious-groups-in-school"  0 example

(* Should be 0, Democrat *)
let classification = TreeClassifier.classify_example g example
let () = Printf.printf "\n\nSingle predicition label: %i\nSingle prediction value: %s\n\n" (classification) (if classification = 0 then "Democrat" else "Republican")

let examples = TreeClassifier.Feature_map.empty
let examples = TreeClassifier.Feature_map.add "handicaped-infants"          [0; 0; 0; 1; 0] examples
let examples = TreeClassifier.Feature_map.add "water-project-cost"          [1; 1; 1; 1; 1] examples
let examples = TreeClassifier.Feature_map.add "adoption-of-the-budget"      [0; 0; 1; 1; 0] examples
let examples = TreeClassifier.Feature_map.add "physician-fee-freeze"        [1; 1; 0; 0; 1] examples
let examples = TreeClassifier.Feature_map.add "el-salvador-aid"             [1; 1; 0; 0; 1] examples
let examples = TreeClassifier.Feature_map.add "religious-groups-in-school"  [0; 1; 0; 1; 1] examples

(* Should be 1, 1, 0, 0, 1 or Rep, Rep, Dem, Dem, Rep *)
let classifications = TreeClassifier.classify_examples g examples
let () = Printf.printf "Multiple predicitions:\n"; 
  List.iter (fun pred -> Printf.printf "predicition label: %i\nprediction value: %s\n" (pred) (if pred = 0 then "Democrat" else "Republican")) classifications

