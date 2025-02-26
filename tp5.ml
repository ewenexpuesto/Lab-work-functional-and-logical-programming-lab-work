(* Arbres quaternaires *)

type color = Black | White;;
type 'a quadtree = Leaf of 'a | Node of 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree;;

Node(Leaf Black, Leaf White, Leaf Black, Leaf White);;

let rec mirrorV : 'a quadtree -> 'a quadtree = fun q -> match q with
  | Leaf x -> Leaf x
  | Node(nw,ne,se,sw) -> Node(mirrorV ne, mirrorV nw, mirrorV sw, mirrorV se)
;;

let rec rotate : 'a quadtree -> 'a quadtree = fun q -> match q with
  | Leaf x -> Leaf x
  | Node(nw,ne,se,sw) -> Node(rotate ne, rotate se, rotate sw, rotate nw)
;;

let rec chessboard : int -> color quadtree = fun n -> match n with
  | 0 -> failwith "n must be greater than 0"
  | 1 -> Node(Leaf Black, Leaf White, Leaf Black, Leaf White)
  | i -> Node(chessboard (i-1), chessboard (i-1), chessboard (i-1), chessboard (i-1))
;;

let rec density : color quadtree -> float = fun q -> match q with
  | Leaf Black -> 1.
  | Leaf White -> 0.
  | Node(nw,ne,se,sw) -> (density nw +. density ne +. density se +. density sw) /. 4. (* 2 times 2 recursively is the number of squares *)
;;

let rec apply : ('a quadtree -> 'a quadtree) list -> 'a quadtree -> 'a quadtree = fun l t0 -> List.fold_left (fun t f -> f t) t0 l;;

let rotate_clockwise : 'a quadtree -> 'a quadtree = fun q -> apply [rotate ; rotate ; rotate] q;;

let mirrorH: 'a quadtree -> 'a quadtree = fun q -> apply [rotate ; rotate ; mirrorV] q;;

(* Expressions arithmétiques *)

type expr =
  | Nb of int
  | Plus of expr * expr
  | Mult of expr * expr
  | Minus of expr * expr
;;

Minus(Mult(Plus(Nb 1, Nb 4), Nb 3), Nb 5);;

let rec nb_op : expr -> int = fun e -> match e with
  | Nb _ -> 0
  | Plus(e1,e2) -> nb_op e1 + nb_op e2 + 1
  | Mult(e1,e2) -> nb_op e1 + nb_op e2 + 1
  | Minus(e1,e2) -> nb_op e1 + nb_op e2 + 1
;;

let rec eval: expr -> int = fun e -> match e with
  | Nb n -> n
  | Plus(e1,e2) -> eval e1 + eval e2
  | Mult(e1,e2) -> eval e1 * eval e2
  | Minus(e1,e2) -> eval e1 - eval e2
;;

let rec nb_null : expr -> int = fun e -> match e with
  | Nb n -> if n = 0 then 1 else 0
  | Plus(e1,e2) -> if eval e1 + eval e2 = 0 then 1 + nb_null e1 + nb_null e2 else nb_null e1 + nb_null e2
  | Mult(e1,e2) -> if eval e1 * eval e2 = 0 then 1 + nb_null e1 + nb_null e2 else nb_null e1 + nb_null e2
  | Minus(e1,e2) -> if eval e1 - eval e2 = 0 then 1 + nb_null e1 + nb_null e2 else nb_null e1 + nb_null e2
;;