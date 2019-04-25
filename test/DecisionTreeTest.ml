open DecisionTreev2

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
let x = DTree.Feature_map.empty
let x = DTree.Feature_map.add "handicaped-infants"          [0; 0; 0; 0; 1; 0; 0; 0; 0] x
let x = DTree.Feature_map.add "water-project-cost"          [1; 1; 1; 1; 1; 1; 1; 1; 1] x
let x = DTree.Feature_map.add "adoption-of-the-budget"      [0; 0; 1; 1; 1; 1; 0; 0; 0] x
let x = DTree.Feature_map.add "physician-fee-freeze"        [1; 1; 0; 0; 0; 0; 1; 1; 1] x
let x = DTree.Feature_map.add "el-salvador-aid"             [1; 1; 1; 0; 1; 1; 1; 1; 1] x
let x = DTree.Feature_map.add "religious-groups-in-school"  [1; 1; 1; 1; 1; 1; 1; 1; 1] x

(* list of examples in the same order as the lists of examples are given in x. *)
let y = [1; 1; 0; 0; 0; 0; 0; 1; 1]

(* Map with key being feature name and value being the number of attributes for this feature.
   All features here are boolean featurues so they only have 0 or 1 which is why they all have 1. *)
let attrs = DTree.Feature_map.empty
let attrs = DTree.Feature_map.add "handicaped-infants" 1 attrs
let attrs = DTree.Feature_map.add "water-project-cost" 1 attrs
let attrs = DTree.Feature_map.add "adoption-of-the-budget" 1 attrs
let attrs = DTree.Feature_map.add "physician-fee-freeze" 1 attrs
let attrs = DTree.Feature_map.add "el-salvador-aid" 1 attrs
let attrs = DTree.Feature_map.add "religious-groups-in-school" 1 attrs;;

let () = DTree.build_tree_v2 x y attrs