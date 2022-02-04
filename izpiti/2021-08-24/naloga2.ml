(*============================================================================*]
  Pri tej nalogi bomo za slovar uporabili kar enostavno implementacijo z 
  asociativnim seznamom, ki smo jo spoznali na predavanjih.
  S spodaj definiranimi funkcijami si lahko pomagate pri vseh podnalogah.
[*============================================================================*)

type ('a, 'b) slovar = ('a * 'b) list

let prazen_slovar : ('a, 'b) slovar = []

let velikost (m : ('a, 'b) slovar) = List.length m

let vsebuje (x : 'a) (m : ('a, 'b) slovar) = List.mem_assoc x m

(* Vrne vrednost, ki pripada ključu ali None *)
let najdi x (m : ('a, 'b) slovar) = List.assoc_opt x m

(* Doda vrednost v slovar in povozi prejšnjo, če obstaja *)
let dodaj (k, v) (m : ('a, 'b) slovar)  = (k, v) :: List.remove_assoc k m

(*============================================================================*]
  Matematične izraze predstavimo z dvojiškimi drevesi, v katerih vozlišča predstavljajo 
  aritmetične operacije, listi pa števila ali spremenljivke, predstavljene z nizi.
  Izraz v drevo pretvorimo tako, da pri operaciji levi podizraz vzamemo za levo 
  poddrevo, desni podizraz za desno, v vozlišče pa zapišemo operator.
[*============================================================================*)

type operator = Plus | Minus | Krat | Deljeno

type 'a izraz =
  | Spremenljivka of string
  | Konstanta of 'a
  | Operacija of ('a izraz * operator * 'a izraz)

(* (x - 3)- (y * (z / x))  *)
let primer =
  Operacija
    ( Operacija (Spremenljivka "x", Minus, Konstanta 3),
      Minus,
      Operacija
        ( Spremenljivka "y",
          Krat,
          Operacija (Spremenljivka "z", Deljeno, Spremenljivka "x") ) )

(* a *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `prestej : izraz -> int`, ki vrne število vseh "različnih" 
  spremenljivk v izrazu.
[*----------------------------------------------------------------------------*)
let unique_list list = 
  let rec aux_unique list acc = 
    match list with
    | [] -> acc
    | x :: xs -> if List.mem x acc then aux_unique xs acc else aux_unique xs (x :: acc)
  in aux_unique list []


let prestej (izraz : 'a izraz) = 
  let rec aux_prestej izraz acc = 
    match izraz with 
    | Operacija (levi_izraz, operacija, desni_izraz) -> aux_prestej levi_izraz acc @ aux_prestej desni_izraz acc
    | Spremenljivka (x) -> x :: acc
    | Konstanta (y) -> acc
  in List.length (unique_list (aux_prestej izraz []))

(* b *)
(*----------------------------------------------------------------------------*]
Napišite funkcijo `izlusci : 'a izraz -> (string * int) slovar`, ki sprejme izraz 
in vrne slovar, ki pove, kolikokrat se posamezna spremenljivka pojavi v izrazu. 
Vrstni red v slovarju ni pomemben.
[*----------------------------------------------------------------------------*)
 
(* c *)
let option_slovar = function 
  | None -> 0
  | Some x -> x

let seznam_v_slovar (list : 'a list) : ('a, int) slovar =
  let rec aux_sez list acc = 
    match list with
    | [] -> acc
    | x :: xs -> let previous = option_slovar (najdi x acc) in let acc = dodaj (x, previous + 1) acc in aux_sez xs acc
  in aux_sez list prazen_slovar
    


let izlusci (izraz : 'a izraz) : (string, int) slovar =
  let rec aux_izlusci izraz acc = 
    match izraz with
    | Operacija (levi_izraz, operacija, desni_izraz) -> aux_izlusci levi_izraz acc @ aux_izlusci desni_izraz acc
    | Spremenljivka (x) -> x :: acc
    | Konstanta (y) -> acc
  in aux_izlusci izraz [] |> seznam_v_slovar


(*----------------------------------------------------------------------------*]
  Napišite funkcijo `izracunaj : (string * int) slovar -> int izraz -> option int`, 
  ki sprejme izraz in slovar vrednosti spremenljivk ter poskuša izračunati vrednost 
  izraza. Če to ni mogoče (deljenje z 0 ali manjkajoča definicija spremenljivke), 
  naj bo rezultat `None`. 
    # izracunaj [("x",3); ("y", 4); ("z",5)] primer;;
    - : int option = Some (-4)
[*----------------------------------------------------------------------------*)

let operacije levi operacija desni = 
  if levi = None || desni = None then None
  else
    let Some x = levi and Some y = desni in
    match operacija with
    | Plus -> Some (x + y)
    | Minus -> Some (x - y)
    | Krat -> Some (x * y)
    | Deljeno -> if y = 0 then None else Some (x / y)

let rec izracunaj (slovar : (string, int) slovar) (izraz : int izraz) : int option =
  match izraz with
  | Operacija (leva_stran, operacija, desni_izraz) -> operacije (izracunaj slovar leva_stran) operacija (izracunaj slovar desni_izraz)
  | Spremenljivka (x) -> najdi x slovar   
  | Konstanta (y) -> Some y
  
(* c *)
(*----------------------------------------------------------------------------*]
  Ocenite časovno zahtevnost funkcije `izracunaj` v odvisnosti od velikosti 
  izraza `n` (torej števila vseh vozlišč in listov v drevesu) ter števila različnih 
  spremenljivk `m`.
  Kako se časovna zahtevnost spremeni, če bi za slovar uporabili uravnoteženo iskalno drevo?
[*----------------------------------------------------------------------------*)
