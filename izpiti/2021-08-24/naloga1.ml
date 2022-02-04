(* a *)
(*----------------------------------------------------------------------------*]
  Napišite predikat `je_urejena : int * int * int -> bool`, ki pove, ali je 
  podana trojica celih števil urejena strogo naraščajoče.
[*----------------------------------------------------------------------------*)
let je_urejena (triple: int * int * int) : bool =
  let (x, y, z) = triple in
  if x < y && y < z then true else false
(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `poskusi_deljenje : float option -> float option -> float option`, 
  ki sprejme morebitni deljenec in morebitni delitelj ter vrne rezultat deljenja, 
  če se to da, ali pa `None`, če ne (kadar kakšnega argumenta ni ali pa bi prišlo 
  do deljenja z nič). 
  
    # poskusi_deljenje (Some 1.0) (Some 2.0);;
    - : float option = Some 0.5
    # poskusi_deljenje (Some 1.0) (Some 0.0);;
    - : float option = None
    # poskusi_deljenje None (Some 2.0);;
    - : float option = None
[*----------------------------------------------------------------------------*)
let poskusi_deljenje (deljenec : float option) (delitelj : float option) : float option = 
  match deljenec with 
    | None -> None
    | Some d -> match delitelj with
                  | None -> None
                  | Some 0.0 -> None
                  | Some l -> Some (d /. l)
(* c *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo `zavrti : 'a list -> int -> 'a list`, ki seznam zavrti 
  za dano število mest v levo (v vsaki rotaciji se prvi element prestavi na 
  konec seznama).
  
    # zavrti [1; 2; 3; 4; 5] 2;;
    - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)
let rec zavrti (list : 'a list) (num : int) : 'a list = 
  match num with
  | 0 -> list
  | n -> match list with
            | [] -> []
            | x :: xs -> zavrti (xs @ [x]) (n - 1)
(* d *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `razdeli : ('a -> int) -> 'a list -> ('a list *  'a list * 'a list)|`, 
  ki sprejme cenilno funkcijo in seznam elementov. Vrne naj trojico, kjer so na prvem 
  mestu vsi elementi za katere je cenilna funkcija negativna, na drugem vsi, kjer 
  je enaka 0, na tretjem pa vsi preostali elementi.
  Elementi naj v seznamih nastopajo v enakem vrstnem redu kot v prvotnem seznamu. 
  Za vse točke naj bo funkcija repno rekurzivna.
  
    # razdeli ((-) 3) [1; 2; 3; 4; 5; 6];;
    - : int list * int list * int list = ([4; 5; 6], [3], [1; 2])
[*----------------------------------------------------------------------------*)
let obrni list = 
 let rec aux list acc = 
  match list with
  | [] -> acc
  | x :: xs -> aux xs (x :: acc) 
 in aux list []

let razdeli (cenilka : 'a -> int) (list : 'a list) : ('a list * 'a list * 'a list) = 
  let rec aux_razdeli cenilka list acc1 acc2 acc3 = 
    match list with
    | [] -> (acc1, acc2, acc3)
    | x :: xs when cenilka x < 0 -> aux_razdeli cenilka xs (x :: acc1) acc2 acc3
    | x :: xs when cenilka x = 0 -> aux_razdeli cenilka xs acc1 (x :: acc2) acc3
    | x :: xs when cenilka x > 0 -> aux_razdeli cenilka xs acc1 acc2 (x :: acc3)
  in aux_razdeli cenilka (obrni list) [] [] []