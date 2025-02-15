(* On commence doucement pour éviter un claquage *)

let rec nbbase : int -> int -> int = fun b n -> 
  if n < 0 then nbbase b (-n)
  else if n < b then 1
  else 1 + nbbase b (n/b)
;;

let _ = nbbase 2 42 = 6;;

(* Listes polymorphes *)

let ajoute : 'a -> 'a list -> 'a list = fun x l ->
  x::l
;;

let rec recherche : 'a -> 'a list -> bool = fun x l -> match l with
  |[] -> false
  |e::l' -> (e=x) || recherche x l'
;;

let _ = recherche 4 [3;4;5] = true;;

let rec existe_pair : int list -> bool = fun l -> match l with
  |[] -> false
  |e::l' -> (e mod 2 = 0) || existe_pair l'
;;

let _ = existe_pair [1;3;5] = false;;
let _ = existe_pair [3;3;2] = true;;

(* doesn't work : e is temporary
let rec supprime : 'a -> 'a list -> 'a list = fun x l -> match l,x with
  |[] -> []
  |e::l',e -> l'
*)

let rec supprime : 'a -> 'a list -> 'a list = fun x l -> match l with
  |[] -> []
  |e::l' when e=x -> l'
  |e::l' -> e::(supprime x l')
;;

let rec supprime : 'a -> 'a list -> 'a list = fun x l -> match l with
  |[] -> []
  |e::l' -> if e = x then l' else e::(supprime x l')
;;

let _ = supprime 1 [1;1;1] = [1;1];;
let _ = supprime 1 [3;1;2;1] = [3;2;1];;

let rec supprime_tout : 'a -> 'a list -> 'a list = fun x l -> match l with
  |[] -> []
  |e::l' when e=x -> supprime_tout x l'
  |e::l' -> e::(supprime_tout x l')
;;

let _ = supprime_tout 1 [1;1;1] = [];;
let _ = supprime_tout 1 [3;1;2;1] = [3;2];;

(* la première version est quadratique *)
let rec inverse : 'a list -> 'a list = fun l -> match l with
  |[] -> []
  |e::l' -> inverse l' @ [e]
;;

(* cette deuxième version est un parcours linéaire *)
let rec inverse_aux : 'a list -> 'a list -> 'a list = fun l accu -> match l with
  |[] -> accu
  |x::q -> inverse_aux q (x::accu)
;;
let inverse : 'a list -> 'a list = fun l -> inverse_aux l [];;

let _ = inverse [1;2;3;4;5]=[5;4;3;2;1];;

let rec existe : ('a -> bool) -> 'a  list -> bool = fun f l -> match l with
  |[] -> false
  |e::l' -> f e || existe f l'
;;

let _ = existe (fun x -> x mod 2 = 1) [2;4;3;8;0];;

(* Listes d'associations *)

let rec recherche_k : 'a -> ('a * 'b) list -> 'b = fun a l -> match l with
  |[] -> raise Not_found
  |(e1,e2)::l' -> if e1 = a then e2 else recherche_k a l'
;;

let rec recherche_k' : 'a -> ('a * 'b) list -> 'b list = fun a l -> match l with
  |[] -> [] (* what to return in this case ? *)
  |(e1,e2)::l' -> if e1 = a then e2::(recherche_k' a l') else recherche_k' a l'
;;

(* Zipper *)

type 'a zipper = {
  left : 'a list;
  right : 'a list;
}

let leftmost : 'a zipper -> bool = fun z -> z.left = [];;
let rightmost : 'a zipper -> bool = fun z -> z.right = [];;
let is_empty : 'a zipper -> bool = fun z -> leftmost z && rightmost z;;

let empty : 'a zipper = {left = [] ; right = []};;

let insert : 'a -> 'a zipper -> 'a zipper = fun x z -> {left = z.left ; right = x::z.right};;

exception Is_leftmost;;
let go_left : 'a zipper -> 'a zipper = fun z -> match z.left with
  |[] -> (* failwith "Cannot go left" *) raise Is_leftmost (* éviter les failwith *)
  |x::q -> {left = q ; right = x::z.right}
;;

(* si {left = [x3,x2,x1] ; right=[x4,x5,x6]} la fonction go_to_left rend {left=[x2,x1],right=[x3,x4,x5,x6]} => ça nous fait aller de x3 à x2*)

exception Is_rightmost
let go_right : 'a zipper -> 'a zipper = fun z -> match z.right with
  |[] -> raise Is_rightmost
  |x::q -> {left = x::z.right ; right = q}
;;

(* gives the convention of the current element *)
let get z = match z.right with
  |[] -> raise Not_found
  |x::_ -> x
;;

let delete z = match z.right with
  |[] -> raise Not_found
  |_::q -> {left = z.left ; right = q}
;;

(* first try : il manque dans le cas d'une liste un peu grande l'étude de la gauche *)
let rec insert_zipper e z = match z.right with
  |[] -> {left = z.left ; right = [e]}
  |e'::zright -> if e'<e then insert_zipper e (go_right z) else {left = z.left ; right = e::z.right}
;;
let z4 = insert_zipper 14 {left = [] ; right = [1;7;28]};;
let _ = z4 = {left = [7;1] ; right = [14;28]};;

(* the following works, you can also pattern match with right and left ; note : the warning is ocaml being wrong with the sub pattern matching *)
(*
let rec insert_zipper e z = match z.right with
  |[] -> match z.left with 
    |[] -> {left = z.left ; right = [e]}
    |e'::zleft -> if e<e' then {left = e::z.left ; right = z.right} else {left = z.left ; right = e::z.right}
  |e'::zright -> if e'<e then insert_zipper e (go_right z) else match z.left with 
    |[] -> {left = z.left ; right = e::z.right}
    |e''::zleft -> if e<e'' then insert_zipper e (go_left z) else {left= z.left ; right = e::z.right}
;;
let z1 = insert_zipper 28 empty;; let _ = z1 = {left = [] ; right = [28]};;
*)

let rec map f l =
  match l with
  | [] -> []
  | h :: t -> f h :: map f t;;

(* print_zipper : inverser d'abord *)
let print_zipper : ('a -> unit) -> 'a zipper -> unit list = fun print z -> map print (inverse z.left @ z.right);;