let hd list =
  match list with
  | Item (elem, items) -> elem
  | _ -> failwith "hd : List is empty"
;;
let rec length list =
  match list with
  | Item (elem, items) -> 1 + length items
  | _ -> 0
;;

let rec nth list x =
  if x > 0 then
    match list with
    | x < 0 || length list < x -> failwith "nth: out of bound"
    | Item (elem, items) -> nth items (x - 1)
    | _ -> failwith "nth : error"
;;
