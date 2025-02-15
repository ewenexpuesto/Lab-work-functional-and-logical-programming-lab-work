(* Listes polymorphes (le retour) *)

let equals e x = e = x;;
(* la fonction suivante renvoie une liste contenant des false et éventuellement un true *)
(* let rec recherche = fun x l -> List.map (equals x) l;; *)

let rec recherche = fun x l -> List.fold_left (fun (* fonction qui a deux arguments *) trouve y -> trouve || y = x) false l;;
let _ = recherche 7 [1;2;3;7];;

let rec existe_pair : int list -> bool = fun l -> List.fold_left (fun trouve y -> trouve || y mod 2 = 0) false l;;
let _ = existe_pair [1;2;3;7];;

(* un fold_left inverserait l'ordre des éléments, mais attention le fold_right est récursif terminal, il vaut mieux utiliser le fold_left quand on peut *)
let rec supprime_tout : 'a -> 'a list -> 'a list = fun x l -> List.fold_right (fun y acc -> if y = x then acc else y::acc) l [];;
let _ = supprime_tout 7 [1;2;7;3;4;7;2];;

(* supprime avec un fold_right est plus subtil car il faut supprimer le premier élément à gauche, l'accumulateur est un couple de liste et on met à jour les deux lsites et c'est la première composante que l'on renvoie *)
let rec supprime : 'a -> 'a list -> 'a list = fun x l -> fst @@ List.fold_right (fun y (s, e) -> if y = x then (e, y::e) else (y::s, y::e)) l ([], []);;  (* avec fst @@ g x qui est équivalent à fst (g x)  alors que fst g x est fst(g)(x)*)
let rec supprime : 'a -> 'a list -> 'a list = fun x l -> List.fold_right (fun y (s, e) -> if y = x then (e, y::e) else (y::s, y::e)) l ([], []) |> fst;; (* fst est une fonction standard qui retourne le premier argument d'un couple  et |> est comme le pipe en bash *)
let rec supprime : 'a -> 'a list -> 'a list = fun x l -> fst (List.fold_right (fun y (s, e) -> if y = x then (e, y::e) else (y::s, y::e)) l ([], []));;

let rec inverse : 'a list -> 'a list = fun l -> List.fold_left (fun acc y -> y::acc) [] l;;
let _ = inverse [2;3;4;5];;

let rec existe : ('a -> bool) -> 'a  list -> bool = fun f l -> List.fold_left (fun trouve y -> trouve || f y) false l;;
(* on pourrait donc utiliser cette fonction existe (prédicat) pour l'existence et la recherche *)