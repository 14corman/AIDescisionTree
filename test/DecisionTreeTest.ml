open TreeClassifier

(* x is a map with the key being the feautre name and the value being a list of examples for that feauture. The original data would look like:
    h  w  a  p  e  r  ...
    0  1  0  1  1  1
    0  1  0  1  1  1
    0  1  1  0  1  1
    0  1  1  0  0  1
    1  1  1  0  1  1
    0  1  1  0  1  1
    ...

    The letters are the abbreviations of the feature names.*)
let x = TreeClassifier.Feature_map.empty
let x = TreeClassifier.Feature_map.add "handicaped-infants"          [0; 0; 0; 0; 1; 0; 0; 0; 0] x
let x = TreeClassifier.Feature_map.add "water-project-cost"          [1; 1; 1; 1; 1; 1; 1; 1; 1] x
let x = TreeClassifier.Feature_map.add "adoption-of-the-budget"      [0; 0; 1; 1; 1; 1; 0; 0; 0] x
let x = TreeClassifier.Feature_map.add "physician-fee-freeze"        [1; 1; 0; 0; 0; 0; 1; 1; 1] x
let x = TreeClassifier.Feature_map.add "el-salvador-aid"             [1; 1; 1; 0; 1; 1; 1; 1; 1] x
let x = TreeClassifier.Feature_map.add "religious-groups-in-school"  [1; 1; 1; 1; 1; 1; 1; 1; 1] x

(* list of examples in the same order as the lists of examples are given in x. *)
let y = [1; 1; 0; 0; 0; 0; 0; 1; 1]

(* Map with key being feature name and value being the number of attributes for this feature.
   All features here are boolean featurues so they only have 0 or 1 which is why they all have 1. *)
let attrs = TreeClassifier.Feature_map.empty
let attrs = TreeClassifier.Feature_map.add "handicaped-infants" 1 attrs
let attrs = TreeClassifier.Feature_map.add "water-project-cost" 1 attrs
let attrs = TreeClassifier.Feature_map.add "adoption-of-the-budget" 1 attrs
let attrs = TreeClassifier.Feature_map.add "physician-fee-freeze" 1 attrs
let attrs = TreeClassifier.Feature_map.add "el-salvador-aid" 1 attrs
let attrs = TreeClassifier.Feature_map.add "religious-groups-in-school" 1 attrs;;

(* Build the decision tree *)
let g = TreeClassifier.build_tree x y attrs in TreeClassifier.print_graph g

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

