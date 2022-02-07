(*============================================================================*]
  Za učinkovitejše iskanje po leksikografsko urejenih parih bomo uporabili
  leksikografska drevesa, ki jih ustvarimo s pomočjo dvojiških dreves.

    type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

  Leksikografsko drevo za pare tipa ['a * 'b] je dvojiško drevo, ki ima v
  vozlišču element tipa ['a] (da lahko primerjamo po prvi komponenti) in pa
  drevo tipa ['b tree] (za primerjanje po drugi komponenti).

    type ('a, 'b) lexi_tree = ('a * 'b tree) tree

  Par [(a, b)] se nahaja v leksikografskem drevesu, če imamo v drevesu vozlišče
  s parom [(a, subtree)] in se [b] nahaja v [subtree]. 

  Primer drevesa za pare (3, "g"), (3, "t"), (7, "a"), (10, "e"), (10, "r"),
  (10, "t") in (10, "z") je:
          
          (7)--------┐
           |   "a"   |
           └---------┘
          /           \
         /             \
    (3)-------┐     (10)-----------┐
     | "g"    |      |     "r"     |
     |    \   |      |    /   \    |
     |    "t" |      |  "e"   "z"  |
     └--------┘      |       /     |
                     |     "t"     |
                     └-------------┘

[*============================================================================*)
type 'a tree = Empty | Node of ('a tree * 'a * 'a tree)
type ('a, 'b) lexi_tree = ('a * 'b tree) tree
let leaf a = Node(Empty, a, Empty)
let three_subtree = Node (Empty, "g", leaf "t")
let ten_subtree = Node (leaf "e", "r", Node (Empty, "z", leaf "t"))

let seven_subtree = leaf "a"



(* a *)
(*============================================================================*]
  Definirajte primer, ki ustreza zgornjemu leksikografskemu drevesu.

[*============================================================================*)
let test_case : (int, string) lexi_tree = Node (leaf (3, three_subtree), (7, seven_subtree), leaf (10, ten_subtree))

(* b *)
(*============================================================================*]
  Napišite funkcijo, ki preveri ali je par prisoten v leksikografskem drevesu.
[*============================================================================*)
let rec prisoten a = function
  | Empty -> false
  | Node (lx, t, rx) when a = t -> a
  | Node (lx, t, rx) when a < t -> prisoten a lx
  | Node (lx, t, rx) -> prisoten a rx

let rec prisoten_lexi (a, b) = function
  | Empty -> false
  | Node (lx, (x, ytree), dx) when a = x -> prisoten b ytree
  | Node (lx, (x, ytree), dx) when a < x -> prisoten_lexi (a, b) lx
  | Node (lx, (x, ytree), dx) -> prisoten_lexi (a, b) dx


(* c *)
(*============================================================================*]
  Napišite funkcijo za vstavljanje elementov v leksikografsko drevo.
[*============================================================================*)

let rec vstavi_element a = function 
  | Empty -> leaf a
  | Node (lx, x, dx) when a = x -> Node (lx, a, dx)
  | Node (lx, x, dx) when a < x -> vstavi_element a lx
  | Node (lx, x, dx) -> vstavi_element a dx


let rec vstavi_element_lex (a, b) drevo = 
  match drevo with
    | Empty -> leaf (a, leaf b)
    | Node (lx, (x, ytree), dx) when x = a -> 
        Node (lx, (x, vstavi_element b ytree), dx)
    | Node (lx, (x, ytree), dx) when a < x -> Node (vstavi_element_lex (a, b) lx, (x, ytree), dx)
    | Node (lx, (x, ytree), dx) -> Node (lx, (x, ytree), vstavi_element_lex (a, b) dx)



(* d *)
(*============================================================================*]
  Napišite funkcijo [lexi_fold], ki sprejme funkcijo [f] in začetno vrednost
  akumulatorja, nato pa funkcijo zloži preko leksikografskega drevesa. Vrstni
  red zlaganja je določen z leksikografsko urejenostjo.

    lexi_fold : ('a -> 'b -> 'c -> 'a) -> 'a -> ('b, 'c) lexi_tree -> 'a
[*============================================================================*)
let rec tree_fold f acc = function
  | Empty -> acc
  | Node (lx, x, dx) -> 
    let left = tree_fold f lx in
    let this = f left x in
    tree_fold f this dx

  
let rec tree_fold_lexi f acc = function
  fold (fun acc (x, ytree) -> ) f 

(* e *)
(*============================================================================*]
  Napišite funkcijo, ki vrne urejen seznam vseh elementov, ki se nahajajo v
  leksikografskem drevesu.
[*============================================================================*)
