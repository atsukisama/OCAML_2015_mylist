type 'a my_list =
  | Item of ('a * 'a my_list)
  | Empty

let hd = function
  | Item (elem, items) -> elem
  | _ -> failwith "hd : List is empty or invalid"
;;

let tl = function
  | Item (elem, items) -> items
  | _ -> failwith "tl : List is empty or invalid"
;;

let rec length = function
  | Item (elem, items) -> 1 + length items
  | _ -> 0
;;

let rec nth list x =
  if x >= 0 then
    match list with
    | Item (elem, items) -> if x > 0 then nth items (x - 1) else elem
    | _ -> failwith "nth : error"
  else
    failwith "nth : error"
;;

(* let rec rev = function *)
(*   let ret *)
(*     | Item (elem, items) -> append (Item (elem, items)) ret *)
(*     | _ -> ret *)

let rec append list1 list2 =
  match list1 with
  | Item(elem, items) -> Item (elem, append items list2)
  | _ -> list2
;;

let rev_append list1 list2 =
  match list2 with
  | Item(elem, items) -> append list2 list1
  | _ -> list2
;;
