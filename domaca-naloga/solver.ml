
type available = { loc : int * int; possible : int list }

type state = { problem : Model.problem; current_grid : int option Model.grid; available_slots : available list}


let copy_state (state : state) : state = 
  {state with current_grid = Model.copy_grid state.current_grid}


let update_slots (state : state) : state =
  let new_slots = List.filter (fun x -> if Option.is_none (state.current_grid.(fst x.loc).(snd x.loc)) then true else false) state.available_slots in
  {state with available_slots = new_slots}


let sort_slots (state : state) : state = 
  let sorted = List.sort (fun x y -> compare (List.length x.possible) (List.length y.possible)) state.available_slots in
  {state with available_slots = sorted}

  
let constrain_slot (state : state) ((row, column) : int * int) : int list = Model.possible_int state.current_grid (row, column)
  

let constrain_slots (state : state) : state = 
  let constrained = List.map (fun x -> {loc = x.loc; possible = constrain_slot state x.loc}) state.available_slots in
  {state with available_slots = constrained}
  	

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

let initialize_state (problem : Model.problem) : state =
  let slots = List.map (fun x -> {loc = x; possible = Model.possible_int problem.initial_grid x}) (Model.available_slots problem.initial_grid) in
  {problem = problem; current_grid = Model.copy_grid problem.initial_grid; available_slots = slots }


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



let branch_state (state : state) : (state * state) option = 
  match state.available_slots with
  | [] -> None
  | x :: xs -> 
    match x.possible with
    | [] -> None
    | y :: ys -> let hypothesis_state = copy_state state and neg_hypothesis = copy_state state in
    hypothesis_state.current_grid.(fst x.loc).(snd x.loc) <- Some y;
    let hypothesis_state = hypothesis_state |> constrain_slots in 
    Some (hypothesis_state, {neg_hypothesis with available_slots = {loc = x.loc; possible = ys} :: xs})


 
(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =

  let state = state |> update_slots |> sort_slots in

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
