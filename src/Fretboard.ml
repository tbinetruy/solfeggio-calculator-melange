open Belt
open Theory

module Tuning = struct
  type t =
    | Standard
    | Ukulele

  let to_string = function
    | Standard -> "standard"
    | Ukulele -> "ukulele"

  let to_notes = function
    | Standard ->
      Note.[e Natural; b Natural; g Natural; d Natural; a Natural; e Natural]
    | Ukulele ->
      Note.[b Natural; e Natural; c Natural; g Natural]
end

module ColoredNote = struct
  type note_info =
    | Interval_color of Note.t * Note.t
    | Scale_color of Note.t

  let to_note = function
    | Interval_color (_, note) -> note
    | Scale_color note -> note

  let interval_to_color = function
    | Interval.Unison | Interval.Octave -> "#416ab0"
    | Interval.Second _ -> "#6290bf"
    | Interval.Third _ -> "#80b0cc"
    | Interval.Fourth _ -> "#9bccd5"
    | Interval.Fifth _ -> "#b6e2dc"
    | Interval.Sixth _ -> "#cff2e0"
    | Interval.Seventh _ -> "#e8fce1"

  let note_info_to_color = function
    | Interval_color (tonic, note) ->
      Interval.from_notes tonic note |. Result.map interval_to_color
    | Scale_color _ -> Result.Ok "#eee"

  let note_info_to_opacity = function
    | Interval_color _ -> "1"
    | Scale_color _ -> "0.5"

  let note_info_to_scale = function
    | Interval_color _ -> "1"
    | Scale_color _ -> "0.75"

  let get_note_style note_info =
    ReactDOM.Style.make
      ~width:"1.5rem"
      ~height:"1.5rem"
      ~borderRadius:"50%"
      ~background:"red"
      ~position:"absolute"
      ~top:"50%"
      ~left:"50%"
      ~transform:("translate(-50%, -50%) scale(" ^ note_info_to_scale note_info ^ ")")
      ~display:"flex"
      ~justifyContent:"center"
      ~alignItems:"center"
      ~fontSize:"0.8rem"
      ~border:"1px solid black"
      ~backgroundColor:(note_info_to_color note_info |. Result.getWithDefault "")
      ~opacity:(note_info_to_opacity note_info)
      ()

  let make ~note_info =
    div ~style:(get_note_style note_info)
      ~children:[React.string (note_info |> to_note |> Note.to_string)] () [@JSX]
  [@@react.component]
end

module GuitarString = struct
  let rec create_string start_height length acc =
    match length with
    | 0 -> acc
    | n ->
      create_string (start_height + 1) (n - 1)
        (acc |. List.concat [start_height mod 12])

  let draw_notes_on_string chord tonic fret_zero_note extra_notes =
    let fret_zero_height = Note.to_semitones fret_zero_note in
    create_string fret_zero_height 13 []
    |. List.map (fun current_semitone ->
      chord
      |. List.map (fun note -> ColoredNote.Interval_color (tonic, note))
      |. List.concat (extra_notes |. List.map (fun note -> ColoredNote.Scale_color note))
      |. List.reduce None (fun acc note_info ->
        match acc with
        | Some _ -> acc
        | None ->
          let note = ColoredNote.to_note note_info in
          let note_semitone =
            let s = Note.to_semitones note in
            if s < 0 then (12 + s) mod 12 else s mod 12
          in
          if note_semitone = current_semitone then Some note_info else None
      )
    )

  let make ~notes ~fret_zero_note ~extra_notes =
    let wrapper_style = ReactDOM.Style.make
      ~width:"2rem" ~height:"2rem"
      ~borderRight:"1px solid black" ~position:"relative" () in
    let string_style = ReactDOM.Style.make
      ~width:"2rem" ~height:"1rem"
      ~borderBottom:"1px solid black" () in
    notes
    |> Notes.get_root
    |. Option.mapWithDefault
      (div ~children:[React.string "Error"] () [@JSX])
      (fun tonic ->
        draw_notes_on_string notes tonic fret_zero_note extra_notes
        |. List.mapWithIndex (fun i note_info ->
          div ~style:wrapper_style ~key:(string_of_int i) ~children:[
            div ~style:string_style ~children:[] () [@JSX];
            (match note_info with
             | Some info -> ColoredNote.createElement ~note_info:info () [@JSX]
             | None -> div ~children:[] () [@JSX]);
          ] () [@JSX]
        )
        |. List.toArray
        |. React.array
      )
  [@@react.component]
end

module FretNumbers = struct
  let rec build_numbers numbers counter acc =
    match numbers with
    | head :: tail ->
      if counter < head then
        build_numbers numbers (counter + 1) (0 :: acc)
      else
        build_numbers tail (counter + 1) (head :: acc)
    | [] -> List.reverse acc

  let make ~numbers =
    let wrapper_style = ReactDOM.Style.make ~display:"flex" () in
    let number_style = ReactDOM.Style.make
      ~display:"flex" ~alignItems:"center" ~justifyContent:"center"
      ~width:"2rem" ~height:"2rem" ~borderRight:"1px solid white" () in
    let numbers_element =
      build_numbers numbers 0 []
      |. List.toArray
      |. Array.mapWithIndex (fun i number ->
        match number with
        | 0 -> div ~style:number_style ~key:(string_of_int i) ~children:[] () [@JSX]
        | n ->
          div ~style:number_style ~key:(string_of_int i)
            ~children:[React.string (string_of_int n)] () [@JSX]
      )
      |. React.array
    in
    div ~style:wrapper_style ~children:[numbers_element] () [@JSX]
  [@@react.component]
end

let draw_fretboard notes tuning extra_notes =
  Tuning.to_notes tuning
  |. List.mapWithIndex (fun i fret_zero_note ->
    div ~style:(ReactDOM.Style.make ~display:"flex" ())
      ~key:(string_of_int i)
      ~children:[
        GuitarString.createElement ~notes ~fret_zero_note ~extra_notes () [@JSX]
      ] () [@JSX]
  )
  |. List.toArray
  |. React.array

let make ~notes ~tuning ?(extra_notes=[]) () =
  div ~children:[
    div ~children:[draw_fretboard notes tuning extra_notes] () [@JSX];
    FretNumbers.createElement ~numbers:[1; 3; 5; 7; 9; 12] () [@JSX];
  ] () [@JSX]
[@@react.component]
