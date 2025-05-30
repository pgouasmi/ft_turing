open Yojson.Basic.Util
open Types
open Machine




(* module IntMap = Map.Make(Int)

type action = LEFT | RIGHT

type transition = {
  read : string;
  to_state : string;
  write : string;
  action : action;
}

type tape = {
  data : string IntMap.t;
  position : int;
  blank : string;
}

type machine_state = {
  tape : tape;
  current_state : string;
} *)






let read_tape tape = 
  match IntMap.find_opt tape.position tape.data with
  | Some symbol -> symbol
  | None -> tape.blank


let get_tape_min_index tape = 
  if IntMap.is_empty tape.data then
    tape.position
  else
    IntMap.min_binding tape.data |> fst


let get_max_index tape = 
  if IntMap.is_empty tape.data then
    tape.position
  else
    IntMap.max_binding tape.data |> fst


let print_tape tape = 
  print_char '[';
  for i = (get_tape_min_index tape) to (String.length Sys.argv.(2)) do
    let symbol = match IntMap.find_opt i tape.data with
      | Some s -> s
      | None -> tape.blank
    in
    if i = tape.position then
      Printf.printf "<%s>" symbol
    else
      Printf.printf "%s" symbol
  done;
  print_string "] "
  (* print_newline () *)


let create_tape input blank = 
  let chars = String.to_seq input |> List.of_seq |> List.map (String.make 1) in
  let data = List.mapi (fun i char -> (i, char)) chars 
             |> List.fold_left (fun acc (pos, char) -> IntMap.add pos char acc) IntMap.empty in
  { data; position = 0; blank }


let write_tape tape symbol = 
  { tape with data = IntMap.add tape.position symbol tape.data }


let process_input input blank alphabet = 
  (* Vérifier que chaque caractère de l'input est dans l'alphabet *)
  String.iter (fun c -> 
    let char_str = String.make 1 c in
    if not (List.mem char_str alphabet) then
      failwith (Printf.sprintf "Character '%s' in input not found in alphabet: [%s]" 
        char_str (String.concat "; " alphabet))
  ) input;
  
  (* Vérifier que le caractère blank n'est pas dans l'input *)
  if String.contains input (String.get blank 0) then
    failwith (Printf.sprintf "Blank symbol '%s' must not be in the input '%s'" 
      blank input);
  
  (* Si toutes les vérifications passent, créer la bande *)
  let tape = create_tape input blank in
  (* let final_tape = write_tape { tape with position = String.length input } "=" in *)
  { tape with position = 0 }


let move_tape tape direction = 
  match direction with
  | LEFT -> { tape with position = tape.position - 1 }
  | RIGHT -> { tape with position = tape.position + 1 }

  let apply_transition machine state transition = 
    let new_tape = write_tape state.tape transition.write in
    let moved_tape = move_tape new_tape transition.action in
    
    (* S'assurer que la nouvelle position existe *)
    let final_tape = 
      if not (IntMap.mem moved_tape.position moved_tape.data) then
        { moved_tape with data = IntMap.add moved_tape.position moved_tape.blank moved_tape.data }
      else
        moved_tape
    in
    
    {
      tape = final_tape;
      current_state = transition.to_state;
    }

let rec simulate machine state = 
  if List.mem state.current_state (machine#get_finals) then begin
    state
  end
  else begin
    print_tape state.tape;
    let current_symbol = read_tape state.tape in
    match machine#find_transition state.current_state current_symbol with
    | Some transition ->
        Printf.printf "(%s, %s) -> (%s, %s, %s)\n" 
          state.current_state current_symbol
          transition.to_state transition.write
          (match transition.action with LEFT -> "LEFT" | RIGHT -> "RIGHT");
        simulate machine (apply_transition machine state transition)
    | None ->
        Printf.printf "No transition for state=%s, symbol=%s\n"
          state.current_state current_symbol;
        state
  end


let () =
  if Array.length Sys.argv <> 3 then
    Printf.printf "Usage: %s <json_file> <input>\n" Sys.argv.(0)
  else
    try
      let json = Yojson.Basic.from_file Sys.argv.(1) in 
      let machine = new machine json in
      let input = Sys.argv.(2) in
      
      (* Créer l'état initial *)
      let initial_tape = process_input input machine#get_blank machine#get_alphabet in
      let initial_state = {
        tape = initial_tape;
        current_state = machine#get_initial;
      } in
      
      (* Printf.printf "Machine: %s\n" machine#get_name;
      Printf.printf "Input: %s\n" input;
      (* Printf.printf "État initial: %s\n" machine#get_initial; *)
      print_tape initial_state.tape; *)
      (* Simulation *)
      let final_state = simulate machine initial_state in
      
      Printf.printf "\n=== RESULT ===\n";
      print_tape final_state.tape;
      print_newline ();
      (* Printf.printf "État final: %s\n" final_state.current_state; *)
      
    with
    | exn ->
        print_endline ("Error: " ^ Printexc.to_string exn)