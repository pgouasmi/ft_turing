open Yojson.Basic.Util

exception ParsingException of string
exception ExecutionException of string

module TransitionMap = Map.Make(String)  (* clé = symbole lu *)
module StateMap = Map.Make(String)      (* clé = nom de l'état *)
module IntMap = Map.Make(Int)

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
}
  
class machine json_data = object(self)
  val name: string = json_data |> member "name" |> to_string |> String.trim
  val alphabet: string list =  json_data |> member "alphabet" |> to_list |> List.map to_string
  val blank: string = json_data |> member "blank" |> to_string
  val states: string list = json_data |> member "states" |> to_list |> List.map to_string
  val initial_state: string = json_data |> member "initial" |> to_string
  val finals: string list = json_data |> member "finals" |> to_list |> List.map to_string
  val transitions = 
    let parse_action = function
      | "LEFT" -> LEFT 
      | "RIGHT" -> RIGHT 
      (* | "STAY" -> STAY *)
      | s -> raise (ParsingException "Action inconnue")
    in
    let parse_transition trans_json = {
      read = trans_json |> member "read" |> to_string;
      to_state = trans_json |> member "to_state" |> to_string;
      write = trans_json |> member "write" |> to_string;
      action = trans_json |> member "action" |> to_string |> parse_action;
    } in
    let transitions_json = json_data |> member "transitions" in
    transitions_json |> to_assoc |> List.fold_left (fun acc (state_name, trans_list) ->
      let state_transitions = trans_list |> to_list |> List.fold_left (fun state_acc trans ->
        let transition = parse_transition trans in
        TransitionMap.add transition.read transition state_acc
      ) TransitionMap.empty in
      StateMap.add state_name state_transitions acc
    ) StateMap.empty

    initializer
      self#parse_attributes


  method parse_alphabet =
    if not (List.for_all (fun s -> String.length s = 1) alphabet) then raise (ParsingException "Alphabet must be composed of single character") else ();


  method parse_blank =
    if String.length blank <> 1 then raise (ParsingException "Blank must be a single character");
    if not (List.mem blank alphabet) then raise (ParsingException "Blank is not in alphabet")


  method parse_name =
    if name = "" then raise (ParsingException "Empty name") else ();


  method parse_current_state = 
    if not (List.mem initial_state states) then raise (ParsingException "Unknown initial state");


  method parse_final_state = 
    if not (List.for_all (fun final -> List.mem final states) finals) then
      raise (ParsingException "Some final states are not defined in the state list")


  method parse_transitions =
    StateMap.iter (fun source_state state_map ->
      if not (List.mem source_state states) then
        raise (ParsingException "État source '%s' dans les transitions non défini");
      TransitionMap.iter (fun read_symbol transition ->
        if not (List.mem transition.to_state states) then
          raise (ParsingException "État destination '%s' (depuis %s) non défini");
        if not (List.mem transition.read alphabet) then
          raise (ParsingException "Symbole lu '%s' (état %s) non défini dans l'alphabet");
        if not (List.mem transition.write alphabet) then
          raise (ParsingException "Symbole écrit '%s' (état %s) non défini dans l'alphabet");
        if transition.read <> read_symbol then
          raise (ParsingException "Incohérence: clé='%s' mais read='%s'")    
      ) state_map
    ) transitions


  method parse_attributes =
    self#parse_name;
    self#parse_alphabet;
    self#parse_blank;
    self#parse_current_state;
    self#parse_final_state;
    self#parse_transitions;


  method find_transition current_state symbol = 
    match StateMap.find_opt current_state transitions with
    | None -> None
    | Some state_map -> TransitionMap.find_opt symbol state_map


  method get_name = name
  method get_blank = blank
  method get_initial = initial_state
  method get_finals = finals

end


let read_tape tape = 
  match IntMap.find_opt tape.position tape.data with
  | Some symbol -> symbol
  | None -> tape.blank

let print_tape tape start_pos end_pos = 
  for i = start_pos to end_pos do
    let symbol = match IntMap.find_opt i tape.data with
      | Some s -> s
      | None -> tape.blank
    in
    if i = tape.position then
      Printf.printf "<%s>" symbol  (* Position actuelle entre crochets *)
    else
      Printf.printf " %s " symbol
  done;
  print_newline ()


let create_tape input blank = 
  let chars = String.to_seq input |> List.of_seq |> List.map (String.make 1) in
  let data = List.mapi (fun i char -> (i, char)) chars 
             |> List.fold_left (fun acc (pos, char) -> IntMap.add pos char acc) IntMap.empty in
  { data; position = 0; blank }


let write_tape tape symbol = 
  { tape with data = IntMap.add tape.position symbol tape.data }


let process_input input blank = 
  (* Traiter l'input pour la machine *)
  let tape = create_tape input blank in
  
  (* Ajouter le symbole = à la fin pour marquer la fin *)
  let final_tape = write_tape { tape with position = String.length input } "=" in
  { final_tape with position = 0 }  (* Remettre la tête au début *)


let move_tape tape direction = 
  match direction with
  | LEFT -> { tape with position = tape.position - 1 }
  | RIGHT -> { tape with position = tape.position + 1 }

  let apply_transition machine state transition = 
  let new_tape = write_tape state.tape transition.write in
  let moved_tape = move_tape new_tape transition.action in
  {
    tape = moved_tape;
    current_state = transition.to_state;
  }

let rec simulate machine state = 
  if List.mem state.current_state (machine#get_finals) then begin
    state
  end
  else begin
    print_tape state.tape 0 10;
    let current_symbol = read_tape state.tape in
    match machine#find_transition state.current_state current_symbol with
    | Some transition ->
        Printf.printf "Étape: %s + %s -> %s/%s/%s\n" 
          state.current_state current_symbol
          transition.to_state transition.write
          (match transition.action with LEFT -> "L" | RIGHT -> "R");
        simulate machine (apply_transition machine state transition)
    | None ->
        Printf.printf "Aucune transition pour état=%s, symbole=%s\n"
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
      let initial_tape = process_input input machine#get_blank in
      let initial_state = {
        tape = initial_tape;
        current_state = machine#get_initial;
      } in
      
      Printf.printf "Machine: %s\n" machine#get_name;
      Printf.printf "Input: %s\n" input;
      Printf.printf "État initial: %s\n" machine#get_initial;
      print_tape initial_state.tape (-2) (String.length input + 3);
      
      (* Simulation *)
      let final_state = simulate machine initial_state in
      
      (* Printf.printf "\n=== RÉSULTAT ===\n"; *)
      print_tape final_state.tape (-5) 15;
      (* Printf.printf "État final: %s\n" final_state.current_state; *)
      
    with
    | exn ->
        print_endline ("Erreur: " ^ Printexc.to_string exn)

(* let () =
  try
    if Array.length Sys.argv <> 3 then
      Printf.printf "Usage: %s <json_file> <input>\n" Sys.argv.(0)
    else begin
        let json = Yojson.Basic.from_file Sys.argv.(1) in
        let machine = new machine json in
        let input = Sys.argv(2) in

        let initial_tape = process_input input machine#get_blank in 
    end
      with
      | ParsingException msg ->
          print_endline msg
      | exn -> 
        print_endline ("An error occured: " ^ Printexc.to_string exn) *)