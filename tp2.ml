(* Enregistrements et couples *)

type rat = {num : int ; den : int};;

let add_rat : rat -> rat -> rat = fun r1 r2 -> 
  {num = r1.num * r2.den + r2.num * r1.den ; den = r1.den * r2.den} (* obligé de mettre num et den *)
;;

let _ = add_rat {num = 1 ; den =2} {num = 2 ; den = 3} = {num =5 ; den = 6};;
let _ = add_rat {num = 1 ; den =2} {num = 2 ; den = 3} = {num =7 ; den = 6};;


type rat_tuple = int * int;;

let add_rat_tuple : rat_tuple -> rat_tuple -> rat_tuple = fun rat_tuple1 rat_tuple2 ->
  let (num1 , den1) = rat_tuple1 in let (num2 , den2) = rat_tuple2 in (num1 * den2 + num2 * den1, den1 * den2)
;;

let add_rat_tuple_2 : rat_tuple -> rat_tuple -> rat_tuple = fun (pn , pd) (qn , qd) -> (pn * qd + qn * pd, pd * qd);;

let _ = add_rat_tuple (1,2) (2,3) = (7, 6);;
let _ = add_rat_tuple_2 (1,2) (2,3) = (7, 6);;

(* Types sommes et types énumérés *)

type nombre = 
  | Nombre of int
  | Flottant of float
;;

let add_nombre = fun n1 n2 -> match n1, n2 with
  | Nombre n1, Nombre n2 -> Nombre (n1 + n2) (* don't forget to add the returned type *)
  | Flottant f1, Flottant f2 -> Flottant (f1 +. f2)
  | Nombre n, Flottant f -> Flottant (float_of_int n +. f)
  | Flottant f, Nombre n -> Flottant (f +. float_of_int n)
;;

let substract_nombre = fun n1 n2 -> match n1, n2 with
  | _, Nombre n2 -> add_nombre n1 (Nombre (-n2))
  | _, Flottant f -> add_nombre n1 (Flottant (-.f))
;;

let mult_nombre = fun n1 n2 -> match n1, n2 with
  | Nombre n1, Nombre n2 -> Nombre (n1 * n2)
  | Flottant f1, Flottant f2 -> Flottant (f1 *. f2)
  | Nombre n, Flottant f -> Flottant (float_of_int n *. f)
  | Flottant f, Nombre n -> Flottant (f *. float_of_int n)
;;

let div_nombre = fun n1 n2 -> match n1, n2 with
  | _, Nombre n2 when n2 <> 0 -> mult_nombre n1 (Nombre (1/n2))
  | _, Flottant f -> mult_nombre n1 (Flottant (1./.f)) (* la division par zéro avec un flottant donne un flottant infini *)
  | _ -> failwith "Cannot divide by zero"
;;

let comparison_nombre = fun n1 n2 -> match n1, n2 with
  | Nombre n1, Nombre n2 -> n1 < n2
  | Flottant f1, Flottant f2 -> f1 < f2
  | Nombre n, Flottant f -> float_of_int n < f (* on pourrait tronquer le float mais ce serait plus compliqué *)
  | Flottant f, Nombre n -> f < float_of_int n
;;

(* Fonctions récursives *)

let rec fib_naive : int -> int = fun n ->
  if n = 0 then 1
  else if n = 1 then 1 (* ne pas oublier le else *)
  else fib_naive (n-1) + fib_naive (n-2)
;; (* Complexité : 2^n *)

let _ = fib_naive 0;;
let _ = fib_naive 1;;
let _ = fib_naive 10;;
let _ = fib_naive 40;;

let rec fib : int -> int = fun n ->
  let rec fib_aux : int -> int * int = fun n -> match n with
    | 0 -> (1 , 0)
    | n -> let (a , b) = fib_aux (n-1) in (a + b , a) (* on rend l'avant dernier dans le calcul de fib(n + 1) + fib(n) *)
  in let (a , b) = fib_aux n in a
;;

let _ = fib 0;;
let _ = fib 1;;
let _ = fib 10;;
let _ = fib 40;;