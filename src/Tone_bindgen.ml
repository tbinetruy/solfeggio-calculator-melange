(* Typesafe Melange bindings for Tone.js *)

external start : unit -> unit Js.Promise.t = "start"
  [@@mel.module "tone"]

external now : unit -> float = "now"
  [@@mel.module "tone"]

module PolySynth = struct
  type t

  external make : unit -> t = "PolySynth"
    [@@mel.new] [@@mel.module "tone"]

  external to_destination : t -> t = "toDestination"
    [@@mel.send]

  external trigger_attack_release : t -> string array -> string -> unit = "triggerAttackRelease"
    [@@mel.send]

  external trigger_attack_release_at : t -> string array -> string -> float -> unit = "triggerAttackRelease"
    [@@mel.send]
end
