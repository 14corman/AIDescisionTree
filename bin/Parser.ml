open DecisionTree ;;
(*open Printf *);;

(* Function to strip whitespace from the front and end of a string *)
let strip_ws str = Str.replace_first (Str.regexp "[ \n\r\x0c\t]+$") "" (Str.replace_first (Str.regexp "^[ \n\r\x0c\t]+") "" str) ;;

(* Equivalent to Map.update for newer OCaml versions, except this doesn't use an option type *)
let update_map (func : 'a -> 'a) (key : string) (map : 'a DTree.Feature_map.t) =
  DTree.Feature_map.add key (func (DTree.Feature_map.find key map)) map
;;

(* Returns the number of keys in a map *)
let map_len map = DTree.Feature_map.fold (fun _ _ c -> c + 1) map 0 ;;

(* Add the row's values to the maps of features *)
let rec add_feats (feature_map : int list DTree.Feature_map.t) (value_map : int DTree.Feature_map.t)
    (feature_domain : int DTree.Feature_map.t DTree.Feature_map.t) (feats : string list) (line : string list) =
  match feats, line with
  | [], [] -> (feature_map, value_map, feature_domain)
  | (x :: xs), (y :: ys) -> (
      if y = "?" then
        add_feats
          (update_map (fun l -> -1 :: l) x feature_map)
          value_map feature_domain xs ys
      else
        match DTree.Feature_map.find_opt y (DTree.Feature_map.find x feature_domain) with
        | Some c ->
          add_feats
            (update_map (fun l -> c :: l) x feature_map)
            value_map feature_domain xs ys
        | None ->
          let updated_domain = update_map (fun map -> DTree.Feature_map.add y ((DTree.Feature_map.find x value_map) + 1) map) x feature_domain in
          add_feats
            (update_map (fun l -> (DTree.Feature_map.find y (DTree.Feature_map.find x updated_domain)) :: l) x feature_map)
            (update_map (fun c -> c + 1) x value_map)
            updated_domain
            xs ys
    )
  | _, _ -> failwith "Invalid input"
;;

(* Parse the given csv into a list of DTree.Feature_map of the feature values,
     a list of the class labels, and a map from the feature names to the number of unique values *)
let parse (file : string) =
  let in_file = open_in file in
  try
    match Str.split (Str.regexp ",") (strip_ws (input_line in_file)) with
    | [] -> failwith (Printf.sprintf "Empty header in %s" file)
    | (_ :: []) -> failwith (Printf.sprintf "Only one column found in %s" file)
    | (_ :: features) -> (
        (* Initialize the maps *)
        let (feature_map,value_counts,feature_domain) = List.fold_right (fun feat (map1,map2,map3) ->
            (DTree.Feature_map.add feat [] map1,
             DTree.Feature_map.add feat ~-1 map2,
             DTree.Feature_map.add feat DTree.Feature_map.empty map3))
            features
            (DTree.Feature_map.empty,DTree.Feature_map.empty, DTree.Feature_map.empty |> DTree.Feature_map.add "class" DTree.Feature_map.empty)
        in

        (* Add the values in the line to the feature maps *)
        let rec readlines feat_map labels val_map feat_domain =
          let line = strip_ws (try input_line in_file with End_of_file -> "") in
          if String.contains line '?' then readlines feat_map labels val_map feat_domain else
          match Str.split (Str.regexp ",") (strip_ws (input_line in_file)) with
          | [] -> close_in in_file ; (feat_map, labels, val_map)
          | (_ :: []) -> close_in in_file ; (feat_map, labels, val_map)
          | (ex_label :: ex_feats) -> (
              let (f_map, v_map, f_domain) = add_feats feat_map val_map feat_domain features ex_feats in
              let class_domain =
                (* See if there was a new class label in this example *)
                let domain = DTree.Feature_map.find "class" f_domain in
                if (DTree.Feature_map.mem ex_label domain) then
                  domain
                else
                  DTree.Feature_map.add ex_label (map_len domain) domain
              in
              readlines f_map ((DTree.Feature_map.find ex_label class_domain) :: labels) v_map (DTree.Feature_map.add "class" class_domain f_domain)
            )
        in
        readlines feature_map [] value_counts feature_domain
      )
  with e ->
    close_in_noerr in_file ;
    raise e
;;

(* let (feature_map, labels, value_map) = parse "../data/house-votes-84.csv" ;; *)

