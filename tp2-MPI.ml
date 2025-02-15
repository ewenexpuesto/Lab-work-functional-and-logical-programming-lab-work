type monnaie =  {centime : int ; euro : int};;

let monnaie_billet = fun m -> match m with
  | m when m.centime <> 0 -> false
  | m when m.euro = 5 || m.euro = 10 || m.euro = 20 || m.euro = 50 -> true
  | _ -> false
;;

let _ = monnaie_billet({centime = 0; euro = 9});;
let _ = monnaie_billet({centime = 0; euro = 10});;
let _ = monnaie_billet({centime = 1; euro = 10});;

let number_of_change = fun n m -> match n, m with
  | n, m when m.euro <> 0 && n mod m.euro <> 0 -> (n / m.euro) + 1
  | n, m when m.euro <> 0 && n mod m.euro = 0 -> (n / m.euro)
  | n, m when m.centime <> 0 && n mod m.centime <> 0 -> ((n / m.centime) + 1) * 100
  | n, m when m.centime <> 0 && n mod m.centime = 0 -> (n / m.centime) * 100
  | _ -> failwith "Should not happen"
;;

let _ = number_of_change 100 {centime = 0; euro = 10};;
let _ = number_of_change 101 {centime = 0; euro = 10};;
let _ = number_of_change 100 {centime = 10; euro = 0};;