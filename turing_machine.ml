open Yojson.Basic.Util

exception ParsingException of string
exception ExecutionException of string

module TransitionMap = Map.Make(String)  (* clé = symbole lu *)
module StateMap = Map.Make(String)      (* clé = nom de l'état *)

type action = LEFT | RIGHT | STAY

type transition = {
  read : string;
  to_state : string;
  write : string;
  action : action;
}

class machine json_data = object(self)
  val name: string = json_data |> member "name" |> to_string |> String.trim
  val alphabet: string list =  json_data |> member "alphabet" |> to_list |> List.map to_string
  val blank: string = json_data |> member "blank" |> to_string
  val states: string list = json_data |> member "states" |> to_list |> List.map to_string
  val current_state: string = json_data |> member "initial" |> to_string
  val finals: string list = json_data |> member "finals" |> to_list |> List.map to_string
  val transitions = 
    let parse_action = function
      | "LEFT" -> LEFT 
      | "RIGHT" -> RIGHT 
      | "STAY" -> STAY
      | s -> failwith ("Action inconnue: " ^ s)
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


  method parse_alphabet =
    if not (List.for_all (fun s -> String.length s = 1) alphabet) then raise (ParsingException "Alphabet must be composed of single character") else ();


  method parse_blank =
    if String.length blank <> 1 then raise (ParsingException "Blank must be a single character");
    if not (List.mem blank alphabet) then raise (ParsingException "Blank is not in alphabet")


  method parse_name =
    if name = "" then raise (ParsingException "Empty name") else ();


  method parse_current_state = 
    if not (List.mem current_state states) then raise (ParsingException "Unknown initial state");


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


  method get_name = name
end


let () =
  try
    if Array.length Sys.argv <> 3 then
      Printf.printf "Usage: %s <json_file> <input>\n" Sys.argv.(0)
    else begin
        let json = Yojson.Basic.from_file Sys.argv.(1) in let machine = new machine json in
        print_endline machine#get_name;
        machine#parse_attributes
    end
      with
      | ParsingException msg ->
          print_endline msg
      | exn -> 
        print_endline ("An error occured: " ^ Printexc.to_string exn)