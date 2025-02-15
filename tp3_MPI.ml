(* Minimum et maximum d'une liste *)

(* naive method *)
let rec maximum : int list -> int = fun l -> match l with
  | [] -> min_int
  | e::q -> if e > maximum q then e else maximum q
;;
(* de manière plus efficace pour ne pas avoir besoin de recalculer deux fois, la version d'avant n'est pas récursive terminale *)
let rec maximum : int list -> int = fun l -> match l with
  | [] -> min_int
  | e::q -> let m = maximum q in if e > m then e else m
;;
(* d'une autre manière en utilisant fold_left *)
let maximum : int list -> int = List.fold_left (fun max y -> if y > max then y else max) min_int;; (* without the fun *)
let maximum : int list -> int = fun l -> List.fold_left (fun max y -> if y > max then y else max) min_int l;;

let _ = maximum [3;2;1;4;2] = 4;;

let rec minimum : int list -> int = fun l -> match l with
  | [] -> max_int
  | e::q -> if e < minimum q then e else minimum q
;;
let _ = minimum [3;2;1;4;2] = 1;;

let min_max : int list -> int * int = fun l -> (maximum l, minimum l);;
(* meilleure version *)
let min_max : int list -> int * int = List.fold_left (fun (min, max) y -> ((if y < min then y else min) (* parenthèses importantes *), (if y > max then y else max))) (max_int, min_int);;

(* Minimum et maximum d'une liste, avec moins de comparaisons *)

let rec tuples : int list -> (int list) * (int list) = fun l -> match l with
  |[] -> ([max_int],[min_int])
  |[e] -> ([e],[e])
  |e1::e2::q -> if e1 > e2 then let (lower, higher) = tuples q in (e2::lower,e1::higher) else let (lower, higher) = tuples q in (e1::lower,e2::higher)
;;

let min_max_divide_tuples = fun l -> let (l1,l2) = tuples l in (minimum l1,maximum l2);;

let _ = min_max_divide_tuples [1;3;2;6;3;7;2;3] = (1,7);;

(* inspo : split sort *)
let rec split lst = 
  match lst with
  | [] -> ([], [])
  | [x] -> ([x], [])
  | x :: y :: rest -> 
      let (left, right) = split rest in
      (x :: left, y :: right)
