
type available = { loc : int * int; possible : int list }

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : Model.problem; current_grid : int option Model.grid; no_future_grid_list : int option Model.grid list}

(*Funkcija vzame trenutno stanje grida in poišče pozicije lokacije, kjer je None*)
let available_slots (state : state) : (int * int) list =
   let state_list = Model.grid_to_list_of_lists state.current_grid in
   let ind_list = List.init 9 (fun x -> List.init 9 (fun y -> (x,y))) in
   let state_ind = Model.combine_and_flat state_list ind_list in 
   List.find_all (fun (x, y) -> if x = None then true else false) state_ind |> List.map (fun (x,y) -> y) 

(*Funkcija sprejme trenutno stanje grida ter lokacijo None elementa in vrne vse možna števila, ki so lahko na lokaciji glede na omejitve*)
let possible_int (state : state) ((row, column) : int * int) : int list=
  let pos_int = List.init 9 (fun x -> x + 1) in
  let used_row = Model.get_row state.current_grid row |> Array.to_list in
  let used_column = Model.get_column state.current_grid column |> Array.to_list in 
  let used_box = Model.get_box state.current_grid (3 * (row / 3) + column mod 3) |> Array.to_list in
  List.filter (fun x -> Bool.not (List.mem (Some x) used_row || List.mem (Some x) used_column || List.mem (Some x) used_box)) pos_int 


let insert_int (state : state) ((row, column) : int * int) (num : int) : state =
  state.current_grid.(row).(column) <- Some num;
  Model.print_grid Model.string_of_cell state.current_grid;
  { problem = state.problem; current_grid = state.current_grid; no_future_grid_list = state.no_future_grid_list}

let insert_no_future (state : state) grid =
  {problem = state.problem; current_grid = state.current_grid; no_future_grid_list = grid :: state.no_future_grid_list }

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

let initialize_state (problem : Model.problem) : state =
  { current_grid = Model.copy_grid problem.initial_grid; problem; no_future_grid_list = []}

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = Model.map_grid Option.get state.current_grid in
    if Model.is_valid_solution state.problem solution then Solved solution
    else Fail state

      (* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
     se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)
let branch_state (state : state) =
  match available_slots state with
  | [] -> None
  | (x,y) :: xs -> Some (insert_int state (x, y) (List.hd (possible_int state (x, y))), state)
 

 
(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
  match validate_state state with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state' ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state'

and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      None
  | Some (st1, st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          solve_state st2 )

let solve_problem (problem : Model.problem) =
  problem |> initialize_state |> solve_state
