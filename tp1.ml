let pi = 4. *. atan 1.;;
let _ = Printf.printf "pi = %g\n" pi;;

let volume = fun r -> 4. *. pi *. r ** 3. /. 3.;;
let _ = Printf.printf "volume = %g\n" (volume 1.);;

let is_even = fun x ->
    x mod 2 = 0;;
let _ = Printf.printf "2 est pair ? %B\n" (is_even 2);;

let pythagorean = fun c1 -> fun c2 -> fun c3 -> 
    c1 *. c1 *. c2 *. c2 = c3 *. c3 || c1 *. c1 *. c3 *. c3 = c2 *. c2 || c2 *. c2 *. c3 *. c3 = c1 *. c1;;
let _ = Printf.printf "Le triangle %d, %d, %d est-il rectangle ? %B\n" 3 4 5 (pythagorean 3. 4. 5.);;

let pythagorean' = fun c1 c2 c3 -> 
    pythagorean c1 c2 c3 || pythagorean c2 c3 c1 || pythagorean c3 c1 c2;;

let derivative = fun x f dx -> (f (x +. dx) -. f x) /. dx;;
let my_deriv = derivative 1e-20;; (* application partielle, curryfiée *)
let _ = Printf.printf "Dérivée de sin en 0 : %g\n" (my_deriv sin 0.);;

let compose = fun f g x -> f (g x);; (* parenthèses obligatoires or fun f g x -> f g x interprets g not as a function *)
let id = compose atan tan;;
let _ = Printf.printf "Valeur de arctan(tan pi) : %g\n" (id pi);;



let rec quick_exponant = fun x p ->
    if p=0 then 1
    else x * quick_exponant x (p-1);;
let _ = Printf.printf "quick_exponant = %d\n" (quick_exponant 2 3);;
let _ = Printf.printf "quick_exponant = %d\n" (quick_exponant 2 300000);;

let faster_exponent_bis x p =
    let rec aux acc x p =
      if p = 0 then acc
      else if p mod 2 = 0 then aux acc (x * x) (p / 2)
      else aux (acc * x) (x * x) (p / 2)
    in
    aux 1 x p;;
    let _ = Printf.printf "faster_exponent_bis = %d\n" (faster_exponent_bis 3 300000);;

let rec faster_exponant = fun x p -> 
    if p = 0 then 1
    else if (p mod 2 = 0) then let tmp = (faster_exponant x (p/2)) in tmp * tmp
    else let tmp = (faster_exponant x ((p-1)/2)) in x * tmp * tmp;; (* don't forget the parenthesis *)
let _ = Printf.printf "faster_exponant = %d\n" (faster_exponant 3 300000);; (* it works this time *)


(* les flèches sont implicitement parethèsées à leur droite *)
let swap = fun (x, y) -> (y, x);;
let truncate = fun x y -> x;;
let turn = fun f x y -> f y x;;
let function1 = fun f (x, y) -> (f x, y);;
let function2 = fun f g x -> g (f x) x;;
(* in function2, a is the type that goes into f and b is its return type, while c is the return type of g *)