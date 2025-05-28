open Yojson.Basic.Util

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