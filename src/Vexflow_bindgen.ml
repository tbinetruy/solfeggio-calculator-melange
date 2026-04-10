(* Typesafe Melange bindings for VexFlow 5.x *)

type context

module Renderer = struct
  type t

  let svg_backend = 2

  external make : Dom.element -> int -> t = "Renderer"
    [@@mel.new] [@@mel.module "vexflow"]

  external get_context : t -> context = "getContext"
    [@@mel.send]

  external resize : t -> int -> int -> unit = "resize"
    [@@mel.send]
end

module Stave = struct
  type t

  external make : int -> int -> int -> t = "Stave"
    [@@mel.new] [@@mel.module "vexflow"]

  external add_clef : t -> string -> t = "addClef"
    [@@mel.send]

  external add_key_signature : t -> string -> t = "addKeySignature"
    [@@mel.send]

  external set_context : t -> context -> t = "setContext"
    [@@mel.send]

  external draw : t -> unit = "draw"
    [@@mel.send]
end

module Accidental = struct
  type t

  external make : string -> t = "Accidental"
    [@@mel.new] [@@mel.module "vexflow"]
end

module StaveNote = struct
  type t
  type config

  external config :
    keys:string array ->
    duration:string ->
    config = ""
    [@@mel.obj]

  external make : config -> t = "StaveNote"
    [@@mel.new] [@@mel.module "vexflow"]

  external add_modifier : t -> Accidental.t -> int -> t = "addModifier"
    [@@mel.send]
end

module Voice = struct
  type t
  type config

  external config :
    num_beats:int ->
    beat_value:int ->
    config = ""
    [@@mel.obj]

  external make : config -> t = "Voice"
    [@@mel.new] [@@mel.module "vexflow"]

  external set_mode : t -> int -> t = "setMode"
    [@@mel.send]

  let soft_mode = 2

  external add_tickables : t -> StaveNote.t array -> t = "addTickables"
    [@@mel.send]

  external draw : t -> context -> Stave.t -> unit = "draw"
    [@@mel.send]
end

module Formatter = struct
  type t

  external make : unit -> t = "Formatter"
    [@@mel.new] [@@mel.module "vexflow"]

  external join_voices : t -> Voice.t array -> t = "joinVoices"
    [@@mel.send]

  external format : t -> Voice.t array -> int -> t = "format"
    [@@mel.send]
end

external apply_accidentals : Voice.t array -> string -> unit = "applyAccidentals"
  [@@mel.scope "Accidental"] [@@mel.module "vexflow"]
