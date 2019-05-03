open CrossValidate ;;

let (_,_,_,err) = cross_validate 10 None "data/breast-cancer.csv" ;;
Printf.printf "%f\n" err ;;
