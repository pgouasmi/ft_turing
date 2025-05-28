open Types
open Yojson.Basic.Util

module TransitionMap = Map.Make(String)
module StateMap = Map.Make(String)




exception ParsingException of string
exception ExecutionException of string

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
      self#parse_attributes;
      self#print_machine_info


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
        raise (ParsingException "Undefined state found");
      TransitionMap.iter (fun read_symbol transition ->
        if not (List.mem transition.to_state states) then
          raise (ParsingException "Undefined state found");
        if not (List.mem transition.read alphabet) then
          raise (ParsingException "Symbol missing from the alphabet found");
        if not (List.mem transition.write alphabet) then
          raise (ParsingException "Symbol missing from the alphabet found");
        if transition.read <> read_symbol then
          raise (ParsingException "Inconsistency between key and value")    
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

  method print_name =
    Printf.printf "**********************************************\n";
    Printf.printf "*                                            *\n";
    Printf.printf "*                 %s                  *\n" name;
    Printf.printf "*                                            *\n";
    Printf.printf "**********************************************\n";
    ()


  method print_alphabet = 
    Printf.printf "Alphabet: [ %s ]\n" (String.concat ", " alphabet)
    
  method print_states = 
    Printf.printf "States : [ %s ]\n" (String.concat ", " states)
    
  method print_initial = 
    Printf.printf "Initial : %s\n" initial_state
    
  method print_finals = 
    Printf.printf "Finals : [ %s ]\n" (String.concat ", " finals)
    
  method print_transitions = 
    StateMap.iter (fun source_state state_map ->
      TransitionMap.iter (fun read_symbol transition ->
        let action_str = match transition.action with
          | LEFT -> "LEFT"
          | RIGHT -> "RIGHT" 
        in
        Printf.printf "(%s, %s) -> (%s, %s, %s)\n"
          source_state
          read_symbol
          transition.to_state
          transition.write
          action_str
      ) state_map
    ) transitions
    
  method print_machine_info = 
    self#print_name;
    self#print_alphabet;
    self#print_states;
    self#print_initial;
    self#print_finals;
    self#print_transitions;
    Printf.printf "*****************************************************************************\n";
    ()
    
  method get_name = name
  method get_blank = blank
  method get_initial = initial_state
  method get_finals = finals
  method get_alphabet = alphabet

end