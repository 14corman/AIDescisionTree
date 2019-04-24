open Printf ;;

module StrMap = Map.Make(String) ;;
type attributeDomain = (string list) StrMap.t ;;
type attribute = string StrMap.t ;;
type distribution = int StrMap.t ;;
type example = { label: string; attributes: attribute } ;;
type dataset = { domain: attributeDomain; examples: example list; classDomain: string list } ;;
type children = node StrMap.t
and node =
  | DNode of (node ref) * (children ref) * string * distribution (* parent, children, attribute split on, distribution *)
  | LNode of string
  | Root
;;


(* Adds 1 to the count for the given attribute in the given map *)
let updateDist ex map =
  match StrMap.find_opt ex.label map with
  | None -> StrMap.add ex.label 1 map
  | Some x -> StrMap.add ex.label (x + 1) map
;;

(* Get a map of the counts of each class label in the dataset *)
let getDistribution (ds : dataset) =
  List.fold_right updateDist ds.examples StrMap.empty
;;

let getLNodeFromDs (ds : dataset) =
  match ds.examples with
  | []        -> None
  | otherwise ->
    let dist = getDistribution ds in
    let count = StrMap.fold (fun k1 v1 v2 -> if v1 > v2 then v1 else v2) dist ~-1 in
    let maxAttrs = StrMap.fold (fun k v l -> if (v = count) then k :: l else l) dist [] in
    if (List.length maxAttrs = 1) then
      Some (LNode (List.nth maxAttrs 0))
    else
      Some (LNode (List.nth maxAttrs (Random.int (List.length maxAttrs))))
;;

let getLNodeFromDist (dist : distribution) =
  let count = StrMap.fold (fun k1 v1 v2 -> if v1 > v2 then v1 else v2) dist ~-1 in
  let maxAttrs = StrMap.fold (fun k v l -> if (v = count) then k :: l else l) dist [] in
  Some (LNode (List.nth maxAttrs (Random.int (List.length maxAttrs))))
;;

let getMaxClassLabel (ds : dataset) =
  let (k, _) = StrMap.fold (fun k v (k2,v2) -> if (v < v2) then (k,v) else (k2,v2))
                 (List.fold_right (fun k map -> StrMap.add k.label (match StrMap.find_opt k.label map with
                                                               | None   -> 1
                                                               | Some x -> x + 1) map)
                                   ds.examples StrMap.empty) ("",-1) in
    k
;;


(* Calculate the entropy of the given list of counts *)
let entropy (count : int) (counts : int list) =
  let invlog2 = 1.0 /. (log 2.0) in
  let count' = 1.0 /. (float_of_int count) in
  ~-.(List.fold_right (fun c tot -> tot +. ((float_of_int c) *. log ((float_of_int c) *. count'))) counts 0.0) *. count' *. invlog2
;;

(* Count the number of times a value occurs for the given attribute *)
let countValOccurences (attr : string) (examples : example list) (value : string) =
  List.fold_right (fun ex c -> if (StrMap.find attr ex.attributes) = value then c + 1 else c) examples 0
;;

let remainder (ds : dataset) (attr : string) =
  let { domain = d; examples = exs; classDomain = cDomain } = ds in
  let invExampleCount = 1.0 /. (float_of_int (List.length ds.examples)) in  (* p + n *)
  (List.fold_right
   (fun attrVal rem ->
     let filteredExs = List.filter (fun ex -> (StrMap.find attr ex.attributes) = attrVal) exs in
     let valCounts = getDistribution { domain=d; examples = filteredExs; classDomain=cDomain } in (* Counts of each class label for the examples with the value attrVal for attr *)
     let labelCounts = StrMap.fold (fun _ v l -> v :: l) valCounts [] in
     let numRelevant = List.fold_right (fun _ c -> c + 1) filteredExs 0 in
     rem +. ((entropy numRelevant labelCounts) *. (float_of_int numRelevant))
   ) (StrMap.find attr ds.domain) 0.0) *. invExampleCount
;;

let rec train (ds : dataset) (parent : node ref) (dep : int) =
  match dep with
  | 0 -> getLNodeFromDs ds
  | n -> (match ds.examples with
    | []    -> (
      match !parent with
      | LNode _ -> failwith "A leaf cannot be a parent"
      | Root -> failwith "Examples must be provided to train"
      | DNode (_,_,_,dist) -> getLNodeFromDist dist
    )
    | attrs -> (
      let dist = getDistribution ds in
      let (maxAttr, maxValue) = StrMap.fold (fun k v (k2, c) -> if v > c then (k, v) else (k2, c)) dist ("",-1) in
      let (minAttr, _) = StrMap.fold (fun attr _ (oldAttr, oldRemainder) ->
                                        let rem = remainder ds attr in
                                        if rem < oldRemainder then
                                          (attr, rem)
                                        else
                                          (oldAttr, oldRemainder)) ds.domain ("", 1.1) in
      if maxValue = StrMap.fold (fun _ v c -> v + c) dist 0 then
        Some (LNode maxAttr)
      else
        let childs = ref StrMap.empty in
        let ret = DNode (parent, childs, minAttr, dist) in
        List.iter
          (fun attr ->
             let newDs = { domain = StrMap.remove minAttr ds.domain; classDomain=ds.classDomain;
                           examples = List.filter (fun ex -> (StrMap.find minAttr ex.attributes) = attr) ds.examples } in
             match train newDs (ref ret) (dep-1) with
             | None -> (match getLNodeFromDist dist with
               | None -> failwith "An error has occurred"
               | Some x -> childs := StrMap.add attr x (!childs)
             )
             | Some x -> childs := StrMap.add attr x (!childs)
          )
          (StrMap.find minAttr ds.domain) ;
        Some ret
    )
  )
;;

let buildBoundedTree (ds : dataset) (d : int) = train ds (ref Root) d
;;
     
let buildTree (ds : dataset) = train ds (ref Root) max_int ;;

let rec tabber (hang : int) = if hang < 1 then Printf.printf "" else (Printf.printf "  " ; (tabber (hang-1))) ;;
let distPrinter (l : distribution) (hang : int) =
  StrMap.iter (fun x y -> tabber hang ; Printf.printf "%s => %d\n" x y) l ;;

let rec printTree (n : node option) (hang : int) =
  match n with
  | None -> Printf.printf "None\n"
  | Some t -> (
    match t with
    | LNode x -> tabber hang ; Printf.printf "{ Leaf => %s }\n" x
    | Root -> tabber hang ; Printf.printf "{ Root }\n"
    | DNode (_, kids, attr, dist) -> (
      tabber hang ; Printf.printf "{\n" ;
      tabber (1+hang) ; Printf.printf "split on: %s\n" attr ;
      tabber (1+hang) ; Printf.printf "dist: [\n" ;
      distPrinter dist (hang+2) ; tabber (1+hang) ; Printf.printf "]\n" ;
      tabber (1+hang) ; Printf.printf "children: [\n" ;
      StrMap.iter (fun x y -> tabber (hang+2) ; Printf.printf "%s:" x ; printTree (Some y) (hang+2)) !kids ;
      tabber (hang+1) ; Printf.printf "]\n" ; tabber (hang) ; Printf.printf "}\n"
    )
  )
;;

let printer (n : node option) = printTree n 0 ;;

(* let query (ex : example) (n : node) = None *)

let cDomain = ["high"; "low"] ;;
let aDomain = StrMap.empty |> StrMap.add "Supervisor" ["Pat"; "Tom"; "Sally"]
                 |> StrMap.add "Operator" ["Joe"; "Sam"; "Jim"] |> StrMap.add "Machine" ["a"; "b"; "c"] |> StrMap.add "Overtime" ["Y"; "N"] ;;


let exs = [
  { label= "high"; attributes= StrMap.empty |> StrMap.add "Supervisor" "Pat" |> StrMap.add "Operator" "Joe" |> StrMap.add "Machine" "a" |> StrMap.add "Overtime" "N" }
; { label= "low"; attributes= StrMap.empty |> StrMap.add "Supervisor" "Pat" |> StrMap.add "Operator" "Sam" |> StrMap.add "Machine" "b" |> StrMap.add "Overtime" "Y" }
; { label= "low"; attributes= StrMap.empty |> StrMap.add "Supervisor" "Tom" |> StrMap.add "Operator" "Jim" |> StrMap.add "Machine" "b" |> StrMap.add "Overtime" "Y" }
; { label= "high"; attributes= StrMap.empty |> StrMap.add "Supervisor" "Pat" |> StrMap.add "Operator" "Jim" |> StrMap.add "Machine" "b" |> StrMap.add "Overtime" "N" }
; { label= "high"; attributes= StrMap.empty |> StrMap.add "Supervisor" "Sally" |> StrMap.add "Operator" "Joe" |> StrMap.add "Machine" "c" |> StrMap.add "Overtime" "N" }
; { label= "low"; attributes= StrMap.empty |> StrMap.add "Supervisor" "Tom" |> StrMap.add "Operator" "Sam" |> StrMap.add "Machine" "c" |> StrMap.add "Overtime" "N" }
; { label= "low"; attributes= StrMap.empty |> StrMap.add "Supervisor" "Tom" |> StrMap.add "Operator" "Joe" |> StrMap.add "Machine" "c" |> StrMap.add "Overtime" "N" }
; { label= "low"; attributes= StrMap.empty |> StrMap.add "Supervisor" "Pat" |> StrMap.add "Operator" "Jim" |> StrMap.add "Machine" "a" |> StrMap.add "Overtime" "Y" }
] ;;

let ds = { domain=aDomain; examples=exs; classDomain=cDomain; } ;;

let t = buildTree ds ;;
printer t ;;

