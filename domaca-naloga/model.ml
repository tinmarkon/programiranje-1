(* Pomožni tip, ki predstavlja mrežo *)

type 'a grid = 'a Array.t Array.t

(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

(*Vse elemente seznama spremeni v string in vrne string*)
let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep

(*Vse elemente gnezdenega seznama najprej združi v seznam z notranjim sepratorjem in potem še vse sezname z zunanjim*)
let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

(*Funkcija sprejme funkcijo, ki tip alpha preslika v string in celo vrstico, na koncu sestavi vrstico med dvema črtama*)
let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"

(* Funckija sprejme funkcijo, ki slika polje v string in sudoku grid in ga izriše v konzoli*)
let print_grid string_of_cell grid =
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big

(* Funkcije za dostopanje do elementov mreže *)

(*Funkcija sprejme sudoku grid in vrstico, vrne pa array vrstice ki gre po vseh stolpcih*)
let get_row (grid : 'a grid) (row_ind : int) = 
  Array.init 9 (fun col_ind -> grid.(row_ind).(col_ind))

(*Funkcija sprejme sudoku grid vrne pa seznam vseh vrstic*)
let rows grid = List.init 9 (get_row grid)

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)

let get_box (grid : 'a grid) (box_ind : int) = 
  Array.init 9 (fun element -> grid.(3 * (box_ind / 3) + element / 3).(3 * (box_ind mod 3) + element mod 3))

let boxes grid = List.init 9 (get_box grid)

(* Funkcije za ustvarjanje novih mrež *)

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = Array.map (Array.map f) grid
  
let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str =
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid }

let string_of_option cell =
  match cell with
  | None -> " "
  | Some x ->  Printf.sprintf "%d" x

let print_problem problem : unit = print_grid (string_of_option) problem.initial_grid

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char str }

(* Model za izhodne rešitve *)

type solution = int grid

let print_solution solution = print_grid (string_of_int) solution

(*Funkcija ki sprejema seznam in pogleda če so v njem kakšne podvojene vrednosti*)
let rec duplicates = function
  | [] -> false
  | x :: xs -> List.exists ((=) x) xs || duplicates xs


(*Funkcija vzame dva seznama seznamov, jih združi in pretvori v seznam tuple-ov*)
let combine_and_flat lst1 lst2 = List.init 9 (fun x -> List.combine (List.nth lst1 x) (List.nth lst2 x)) |> List.flatten

(*Funkcija sprejema grid in vrne seznam seznamov. Funkcijo sem napisal, ker v tej verziji Array.combine še ne obstaja*)
let grid_to_list_of_lists (grid: 'a grid) : 'a list list =
  grid |> Array.to_list |> List.map (Array.to_list)


(*Funkcija sprejme rešen sudoku in pogleda da je vsota točna*)
let valid_sudoku_sum (grid : solution) : bool = grid |> Array.to_list |> List.map (Array.fold_left (+) 0) |> List.fold_left (+) 0 = 405


(*Funckija sprejme seznam arrayjev in za vsak array pogleda ali obstajajo duplikati. Če jih ni v nobenem vrne true*)
let valid_sequence list = list |> List.map (Array.to_list) |> List.map duplicates |> List.for_all ((=) false) 


(*Funkcija vzame začetni problem in pogleda ali se na vsaki poziciji v rešitvi številke ujemajo z številkami problema (če seveda obstajajo).*)
let solution_from_problem (problem : problem) (solution : solution) : bool = 
  let lst1 = grid_to_list_of_lists problem.initial_grid in
  let lst2 = grid_to_list_of_lists solution in
  let rec checking_tuples list =
    match list with
    | [] -> true
    | (Some x, y) :: xs -> if x = y then checking_tuples xs else false
    | (None, y) :: xs -> checking_tuples xs
  in checking_tuples (combine_and_flat lst1 lst2) 
    

(*Funckija sprejme začetni problem in rešitev, ter pogleda vse potrebne/zadostne pogoje*)
let is_valid_solution (problem : problem) (solution : solution) : bool = 
  valid_sudoku_sum solution && valid_sequence (boxes solution) && valid_sequence (columns solution) && valid_sequence (rows solution) && solution_from_problem problem solution


(* Funkciji za nadaljne operacije na gridih *)
let available_slots (grid : 'a grid) : (int * int) list =
  let problem_list = grid_to_list_of_lists grid in
  let ind_list = List.init 9 (fun x -> List.init 9 (fun y -> (x,y))) in
  let state_ind = combine_and_flat problem_list ind_list in 
  List.find_all (fun (x, y) -> if x = None then true else false) state_ind |> List.map (fun (x,y) -> y) 


let possible_int (grid : 'a grid) ((row, column) : int * int) : int list=
  let pos_int = List.init 9 (fun x -> x + 1) in
  let used_row = get_row grid row |> Array.to_list in
  let used_column = get_column grid column |> Array.to_list in 
  let used_box = get_box grid (3 * (row / 3) + (column / 3)) |> Array.to_list in
  List.filter (fun x -> Bool.not (List.mem (Some x) used_row || List.mem (Some x) used_column || List.mem (Some x) used_box)) pos_int 