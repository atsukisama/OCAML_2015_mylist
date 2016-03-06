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

let rec append list1 list2 =
  match list1 with
  | Item(elem, items) -> Item (elem, append items list2)
  | _ -> list2
;;

let rec rev list =
  match list with
    | Item(elem, items) -> append (rev items) (Item(elem, Empty))
    | _ -> Empty
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

let rec fold_left f a list =
  match list with
  | Item(elem, items) -> (fold_left f (f a elem) items)
  | _ -> a
;;

let rec for_all p list =
  match list with
  | Item(elem, items) -> (p elem) && (for_all p items)
  | _ -> true
;;

let rec exists p list =
  match list with
  | Item(elem, items) -> if (p elem) then true else (exists p items)
  | _ -> false
;;

let rec mem a list =
  match list with
  | Item(elem, items) -> if (a = elem) then true else (mem a items)
  | _ -> false
;;

let rec memq a list =
  match list with
  | Item(elem, items) -> if (a == elem) then true else (memq a items)
  | _ -> false
;;

let rec filter a list =
  match list with
  | Item(elem, items) -> if (a = elem) then Item(elem, filter a items) else (filter a items)
  | _ -> Empty
;;

let rec mem_assoc a list =
  match list with
  | Item((elem1, elem2), items) -> if (elem1 = a) then true else (mem_assoc a items)
  | _ -> false
;;

let rec assoc a list =
  match list with
  | Item((elem1, elem2), items) -> if (elem1 = a) then elem2 else (assoc a items)
  | _ -> raise Not_found
;;

let rec split = function
  | Item((elem1, elem2), items) -> begin match (split items) with
                                   | (a, b) -> (Item(elem1, a), Item(elem2, b)) end
  | _ -> (Empty, Empty)
;;

let rec remove_assoc a list =
  match list with
  | Item((elem1, elem2), items) -> if (elem1 = a) then items else (Item((elem1, elem2), remove_assoc a items))
  | _ -> Empty
;;

