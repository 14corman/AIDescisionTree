(*open TreeClassifier
include Feature_map*)
open Printf ;;

module Feature_map = Map.Make(String) ;;

(* Function to strip whitespace from the front and end of a string *)
let strip_ws str = Str.replace_first (Str.regexp "\\s+$") "" (Str.replace_first (Str.regexp "^\\s+") "" str) ;;

(* Equivalent to Map.update for newer OCaml versions, except this doesn't use an option type *)
let update_map (func : 'a -> 'a) (key : string) (map : 'a Feature_map.t) =
  Feature_map.add key (func (Feature_map.find key map)) map
;;

(* Returns the number of keys in a map *)
let map_len map = Feature_map.fold (fun _ _ c -> c + 1) map 0 ;;

(* Add the row's values to the maps of features *)
let rec add_feats (feature_map : int list Feature_map.t) (value_map : int Feature_map.t)
                    (feature_domain : int Feature_map.t Feature_map.t) (feats : string list) (line : string list) =
  match feats, line with
  | [], [] -> (feature_map, value_map, feature_domain)
  | (x :: xs), (y :: ys) -> (
    if y = "?" then
      add_feats
        (update_map (fun l -> -1 :: l) x feature_map)
        value_map feature_domain xs ys
    else 
      match Feature_map.find_opt y (Feature_map.find x feature_domain) with
      | Some c ->
        add_feats
          (update_map (fun l -> c :: l) x feature_map)
          value_map feature_domain xs ys
      | None ->
        let updated_domain = update_map (fun map -> Feature_map.add y (Feature_map.find x value_map) map) x feature_domain in
        add_feats
          (update_map (fun l -> (Feature_map.find y (Feature_map.find x updated_domain)) :: l) x feature_map)
          (update_map (fun c -> c + 1) x value_map)
          updated_domain
          xs ys
  )
  | x, y -> failwith "Invalid input"
;;

let parse (file : string) =
  let in_file = open_in file in
  match Str.split (Str.regexp ",") (strip_ws (input_line in_file)) with
  | [] -> failwith (Printf.sprintf "Empty header in %s" file)
  | (x :: []) -> failwith (Printf.sprintf "Only one column found in %s" file)
  | (label :: features) -> (
    let (feature_map,value_counts,feature_domain) = List.fold_right (fun feat (map1,map2,map3) ->
                                                                       (Feature_map.add feat [] map1,
                                                                        Feature_map.add feat 0 map2,
                                                                        Feature_map.add feat Feature_map.empty map3))
                                                                    features
                                                                    (Feature_map.empty,Feature_map.empty,Feature_map.empty)
    in

    (* Add the values in the line to the feature maps *)
    let rec readlines feat_map labels val_map feat_domain =
      match Str.split (Str.regexp ",") (strip_ws (input_line in_file)) with
      | [] -> (feat_map, labels, val_map)
      | (x :: []) -> (feat_map, labels, val_map)
      | (ex_label :: ex_feats) ->
        let (f_map, v_map, f_domain) = add_feats feat_map val_map feat_domain features ex_feats in
        readlines f_map (ex_label :: labels) v_map f_domain
    in
    Printf.printf "%s" label ;
    Feature_map.iter (fun k v -> Printf.printf "\n%s" k) feature_map ;
    readlines feature_map [] value_counts feature_domain
  )
;;

(* let (feature_map, labels, value_map) = parse "../data/house-votes-84.csv" ;;
*)
