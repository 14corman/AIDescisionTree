open DecisionTreev2;;

let x = DTree.Feature_map.empty
let x = DTree.Feature_map.add "handicaped-infants"          [0; 0; 0; 0; 1; 0; 0; 0; 0] x
let x = DTree.Feature_map.add "water-project-cost"          [1; 1; 1; 1; 1; 1; 1; 1; 1] x
let x = DTree.Feature_map.add "adoption-of-the-budget"      [0; 0; 1; 1; 1; 1; 0; 0; 0] x
let x = DTree.Feature_map.add "physician-fee-freeze"        [1; 1; 0; 0; 0; 0; 1; 1; 1] x
let x = DTree.Feature_map.add "el-salvador-aid"             [1; 1; 1; 0; 1; 1; 1; 1; 1] x
let x = DTree.Feature_map.add "religious-groups-in-school"  [1; 1; 1; 1; 1; 1; 1; 1; 1] x;;

let y = [1; 1; 0; 0; 0; 0; 0; 1; 1];;

let attrs = DTree.Feature_map.empty
let attrs = DTree.Feature_map.add "handicaped-infants" 1 attrs
let attrs = DTree.Feature_map.add "water-project-cost" 1 attrs
let attrs = DTree.Feature_map.add "adoption-of-the-budget" 1 attrs
let attrs = DTree.Feature_map.add "physician-fee-freeze" 1 attrs
let attrs = DTree.Feature_map.add "el-salvador-aid" 1 attrs
let attrs = DTree.Feature_map.add "religious-groups-in-school" 1 attrs;;

let () = DTree.build_tree_v2 x y attrs