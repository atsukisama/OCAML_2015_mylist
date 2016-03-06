type 'a my_list =
  | Item of ('a * 'a my_list)
  | Empty

let hd = function
  | Item(elem, items) -> elem
  | _ -> failwith "hd : List is empty or invalid"
;;

let tl = function
  | Item(elem, items) -> items
  | _ -> failwith "tl : List is empty or invalid"
;;

let rec length = function
  | Item(elem, items) -> 1 + length items
  | _ -> 0
;;

let rec nth list x =
  if x >= 0 then
    match list with
    | Item(elem, items) -> if x > 0 then nth items (x - 1) else elem
    | _ -> failwith "nth : error"
  else
    failwith "nth : error"
;;

(* let rec rev list = *)
(*   match list with *)
(*     | Item (elem, items) -> rev_append items list *)
(*     | _ -> Item(elem, items) *)

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

let rec flatten = function
  | Item(elem, items) -> append elem (flatten items)
  | _ -> Empty
;;

let rec iter func list =
  match list with
  | Item(elem, items) -> func elem ; iter func items
  | _ -> ()
;;

let rec map func list =
  match list with
  | Item(elem, items) -> Item(func elem, map func items)
  | _ -> Empty
;;


  (* let rec fold_left *)

let rec for_all p list =
  match list with
  | Item(elem, items) -> if (p elem) && (for_all p items) then true else false
  | _ -> false
;;

let rec exists p list =
  match list with
  | Item(elem, items) -> if (p elem) || (for_all p items) then true else false
  | _ -> false
;;

let rec mem a list =
  match list with
  | Item(elem, items) -> if (a = elem) || (mem a items) then true else false
  | _ -> false
;;

let rec memq a list =
  match list with
  | Item(elem, items) -> if (a == elem) || (memq a items) then true else false
  | _ -> false
;;
