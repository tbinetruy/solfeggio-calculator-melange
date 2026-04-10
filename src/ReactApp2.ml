open Belt
open Theory

module StringMap = Stdlib.Map.Make(String)

module Select = struct
  let get_options spec =
    []
    |> StringMap.fold (fun value _ acc ->
        option ~key:value ~children:[React.string value] () [@JSX] :: acc
      ) spec
    |> Stdlib.List.rev
    |> Stdlib.Array.of_list
    |> React.array

  let on_change spec event =
    let target = React.Event.Form.target event in
    let _ =
      spec
      |> StringMap.find_opt target##value
      |> Stdlib.Option.map (fun callback -> callback ()) in
    ()

  let make ~spec ~value =
    div ~children:[
      select ~value ~onChange:(on_change spec) ~children:[get_options spec] () [@JSX]
    ] () [@JSX]
  [@@react.component]
end


module Styles = struct
  let app = ReactDOM.Style.make
    ~maxWidth:"960px" ~margin:"0 auto" ~padding:"1.5rem"
    ~fontFamily:"-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif"
    ~color:"#1a1a2e" ()

  let title = ReactDOM.Style.make
    ~fontSize:"1.8rem" ~fontWeight:"700" ~marginBottom:"0.25rem" ()

  let link = ReactDOM.Style.make
    ~fontSize:"0.85rem" ~color:"#666" ~textDecoration:"none"
    ~marginBottom:"1.5rem" ~display:"block" ()

  let controls = ReactDOM.Style.make
    ~display:"flex" ~flexWrap:"wrap" ~gap:"0.75rem 1.5rem"
    ~marginBottom:"1.5rem" ~padding:"1rem"
    ~backgroundColor:"#f8f9fa" ~borderRadius:"8px" ()

  let control_row = ReactDOM.Style.make
    ~display:"flex" ~alignItems:"center" ~gap:"0.4rem" ()

  let label = ReactDOM.Style.make
    ~fontSize:"0.85rem" ~fontWeight:"600" ~color:"#555"
    ~textTransform:"uppercase" ~letterSpacing:"0.03em" ()

  let section = ReactDOM.Style.make
    ~marginBottom:"1.5rem" ~padding:"1rem"
    ~border:"1px solid #e0e0e0" ~borderRadius:"8px"
    ~backgroundColor:"#fafafa" ()

  let section_title = ReactDOM.Style.make
    ~fontSize:"0.9rem" ~fontWeight:"700" ~color:"#444"
    ~marginBottom:"0.5rem" ~textTransform:"uppercase"
    ~letterSpacing:"0.05em" ()

  let chord_list = ReactDOM.Style.make
    ~fontFamily:"'SF Mono', 'Fira Code', 'Consolas', monospace"
    ~fontSize:"0.95rem" ~lineHeight:"1.6" ~letterSpacing:"0.02em" ()

  let fretboard_card = ReactDOM.Style.make
    ~marginBottom:"1.5rem" ~padding:"1rem"
    ~border:"1px solid #ddd" ~borderRadius:"8px" ()

  let chord_header = ReactDOM.Style.make
    ~fontSize:"1.1rem" ~fontWeight:"700" ~marginBottom:"0.25rem" ()

  let interval_line = ReactDOM.Style.make
    ~fontSize:"0.85rem" ~color:"#666" ~fontFamily:"'SF Mono', 'Fira Code', monospace" ()
end

module FlexRow = struct
  let make ~children =
    div ~style:Styles.control_row ~children:[children] () [@JSX]
  [@@react.component]
end


module AnnotatedFretboard = struct
  let roman_numeral = function
    | 0 -> "I" | 1 -> "II" | 2 -> "III" | 3 -> "IV"
    | 4 -> "V" | 5 -> "VI" | 6 -> "VII" | _ -> "?"

  let make ~notes ~tuning ~key_signature ?(degree=None) ?(extra_notes=[]) () =
    let chord_name =
      notes
      |> Intervals.absolute_intervals_of_notes
      |> Chord.quality_of_intervals
      |. Result.mapWithDefault "" (fun quality ->
          " (" ^ Chord.quality_to_string quality ^ ")")
    in
    let prefix = match degree with
      | Some d -> roman_numeral d ^ ": "
      | None -> ""
    in
    div ~style:Styles.fretboard_card ~children:[
      div ~style:Styles.chord_header ~children:[React.string (prefix ^ Notes.string_of_notes notes ^ chord_name)] () [@JSX];
      div ~style:Styles.interval_line ~children:[React.string (notes |> Intervals.relative_intervals_of_notes |> Intervals.to_string)] () [@JSX];
      div ~style:Styles.interval_line ~children:[React.string (notes |> Intervals.absolute_intervals_of_notes |> Intervals.to_string)] () [@JSX];
      Staff.createElement ~notes ~key_signature () [@JSX];
      Fretboard.createElement ~notes ~tuning ~extra_notes () [@JSX];
    ] () [@JSX]
  [@@react.component]
end


module App = struct
  let make () =
    let (root_pitch_class, set_root_pitch_class) = React.useState (fun _ -> Note.c Accidental.Natural) in
    let (accidental, set_accidental) = React.useState (fun _ -> Accidental.Natural) in
    let (chord_quality, _set_chord_quality) = React.useState (fun _ -> None) in
    let (scale_quality, _set_scale_quality) = React.useState (fun _ -> Some Scale.Harmonic_minor) in
    let (interval_type, _set_interval_type) = React.useState (fun _ -> None) in
    let (progression_type, _set_progression_type) = React.useState (fun _ -> (Some [|1; 4; 0|])) in
    let (tuning, set_tuning) = React.useState (fun _ -> Fretboard.Tuning.Standard) in
    let root = Note.set_accidental root_pitch_class accidental in

    let set_chord_quality f =
      _set_chord_quality f;
      _set_interval_type (fun _ -> None);
      _set_scale_quality (fun _ -> None)
    in
    let set_scale_quality f =
      _set_scale_quality f;
      _set_interval_type (fun _ -> None);
      _set_chord_quality (fun _ -> None)
    in
    let set_interval_type f =
      _set_interval_type f;
      _set_scale_quality (fun _ -> None);
      _set_chord_quality (fun _ -> None)
    in

    let notes = match (interval_type, chord_quality, scale_quality) with
      | (Some interval, _, _) -> Interval.to_notes root interval
      | (None, Some quality, _) -> Chord.to_notes root quality
      | (None, None, Some quality) -> Scale.to_notes root quality
      | (None, None, None) -> []
    in

    let key_signature = match scale_quality with
      | Some quality -> Scale.to_vexflow_key root quality
      | None -> "C"
    in

    let harmonization_triad_chords = scale_quality |. Option.mapWithDefault "" (fun quality ->
      match Harmonization.to_triads quality with
      | Result.Ok chords ->
        chords |. List.map Chord.quality_to_string |> String.concat " | "
      | Result.Error msg -> msg
    ) in

    let harmonization_triads = scale_quality |. Option.mapWithDefault "" (fun quality ->
      match Harmonization.to_triad_progression quality root with
      | Result.Ok harmonization -> Progression.to_string harmonization
      | Result.Error msg -> msg
    ) in

    let harmonization_tetrad_chords = scale_quality |. Option.mapWithDefault "" (fun quality ->
      match Harmonization.to_tetrads quality with
      | Result.Ok chords ->
        chords |. List.map Chord.quality_to_string |> String.concat " | "
      | Result.Error msg -> msg
    ) in

    let harmonization_tetrads = scale_quality |. Option.mapWithDefault "" (fun quality ->
      match Harmonization.to_tetrad_progression quality root with
      | Result.Ok harmonization -> Progression.to_string harmonization
      | Result.Error msg -> msg
    ) in

    let tuning_spec =
      [|Fretboard.Tuning.Standard; Ukulele|]
      |. Array.reduce StringMap.empty (fun acc el ->
        acc |> StringMap.add (Fretboard.Tuning.to_string el)
          (fun () -> set_tuning (fun _ -> el)))
    in

    let root_pitch_spec =
      Note.[c Natural; d Natural; e Natural; f Natural; g Natural; a Natural; b Natural]
      |> Stdlib.List.fold_left (fun acc el ->
        acc |> StringMap.add (Note.to_string el)
          (fun () -> set_root_pitch_class (fun _ -> el))
      ) StringMap.empty
    in

    let accidental_spec =
      Accidental.[Flat; Natural; Sharp]
      |> Stdlib.List.fold_left (fun acc el ->
        acc |> StringMap.add (Accidental.to_string el)
          (fun () -> set_accidental (fun _ -> el))
      ) StringMap.empty
    in

    let interval_type_spec =
      let open Interval in
      let open Interval.MajorMinorQuality in
      let open Interval.PerfectQuality in
      [
        Some (Minor |. Second);
        Some (Major |. Second);
        Some (Diminished |. Third);
        Some (Minor |. Third);
        Some (Major |. Third);
        Some (Diminished |. Fourth);
        Some (Perfect |. Fourth);
        Some (Augmented |. Fourth);
        Some (Diminished |. Fifth);
        Some (Perfect |. Fifth);
        Some (Augmented |. Fifth);
        Some (Minor |. Sixth);
        Some (Major |. Sixth);
        Some (Diminished |. Seventh);
        Some (Minor |. Seventh);
        Some (Major |. Seventh);
        None;
      ]
      |> Stdlib.List.fold_left (fun acc el ->
        acc |> StringMap.add
          (el |> Stdlib.Option.map Interval.to_string |> Stdlib.Option.value ~default:"None")
          (fun () -> set_interval_type (fun _ -> el))
      ) StringMap.empty
    in

    let chord_quality_spec =
      let open Chord in
      [
        Some Major_triad; Some Minor_triad; Some Augmented_triad; Some Diminished_triad;
        Some Suspended_triad; Some Power_chord; Some Diminished_power_chord; Some Augmented_power_chord;
        Some Major_sixth; Some Minor_sixth; Some Major_six_nine; Some Minor_six_nine;
        Some Major_seventh; Some Dominant_seventh; Some Minor_seventh_major; Some Minor_seventh;
        Some Augmented_major_seventh; Some Half_diminished_seventh; Some Diminished_seventh;
        Some Suspended_seventh; Some Seventh_augmented_fifth; Some Seventh_diminished_fifth;
        Some Major_ninth; Some Dominant_ninth; Some Minor_ninth; Some Minor_major_ninth;
        Some Dominant_eleventh; Some Minor_eleventh;
        Some Dominant_thirteenth; Some Minor_thirteenth;
        None;
      ]
      |> Stdlib.List.fold_left (fun acc el ->
        acc |> StringMap.add
          (el |> Stdlib.Option.map Chord.quality_to_string |> Stdlib.Option.value ~default:"None")
          (fun () -> set_chord_quality (fun _ -> el))
      ) StringMap.empty
    in

    let scale_quality_spec =
      let open Scale in
      [
        Some Major; Some Natural_minor; Some Harmonic_minor;
        Some Ionian; Some Dorian; Some Phrygian; Some Lydian;
        Some Mixolydian; Some Aeolian; Some Locrian;
        None;
      ]
      |> Stdlib.List.fold_left (fun acc el ->
        acc |> StringMap.add
          (el |> Stdlib.Option.map Scale.quality_to_string |> Stdlib.Option.value ~default:"None")
          (fun () -> set_scale_quality (fun _ -> el))
      ) StringMap.empty
    in

    let string_of_progression_type degrees =
      degrees
      |. Array.map (fun n -> string_of_int (n + 1))
      |> Stdlib.Array.to_list
      |> String.concat "-"
    in

    let progression_type_spec =
      [
        Some [|1; 4; 0|];
        Some [|0; 1; 2; 3; 4; 5; 6|];
        Some [|0; 6; 3|];
        None;
      ]
      |> Stdlib.List.fold_left (fun acc el ->
        acc |> StringMap.add
          (el |> Stdlib.Option.map string_of_progression_type |> Stdlib.Option.value ~default:"None")
          (fun () -> _set_progression_type (fun _ -> el))
      ) StringMap.empty
    in

    let progression =
      (match (scale_quality, progression_type) with
       | (Some quality, Some degrees) ->
         let chords = Harmonization.to_progression quality root Harmonization.tetrad_degrees degrees
           |. Result.getWithDefault [] in
         List.zip chords (degrees |. List.fromArray)
       | _ -> [])
      |. List.mapWithIndex (fun i (chord_notes, deg) ->
        let extra_notes = Notes.subtract notes chord_notes in
        AnnotatedFretboard.createElement
          ~notes:chord_notes ~tuning ~key_signature ~degree:(Some deg) ~extra_notes
          ~key:(string_of_int i) () [@JSX]
      )
      |. List.toArray
      |. React.array
    in

    let mk_label text =
      span ~style:Styles.label ~children:[React.string text] () [@JSX]
    in

    let harmonization = match scale_quality with
      | Some _ ->
        div ~children:[
          div ~style:Styles.section ~children:[
            div ~style:Styles.section_title ~children:[React.string "Triad harmonization"] () [@JSX];
            div ~style:Styles.chord_list ~children:[React.string harmonization_triad_chords] () [@JSX];
            div ~style:Styles.chord_list ~children:[React.string harmonization_triads] () [@JSX];
          ] () [@JSX];
          div ~style:Styles.section ~children:[
            div ~style:Styles.section_title ~children:[React.string "Tetrad harmonization"] () [@JSX];
            div ~style:Styles.chord_list ~children:[React.string harmonization_tetrad_chords] () [@JSX];
            div ~style:Styles.chord_list ~children:[React.string harmonization_tetrads] () [@JSX];
          ] () [@JSX];
        ] () [@JSX]
      | None -> div ~children:[] () [@JSX]
    in

    div ~style:Styles.app ~children:[
      h1 ~style:Styles.title ~children:[React.string "Solfeggio Calculator"] () [@JSX];
      a ~style:Styles.link ~href:"https://github.com/tbinetruy/solfeggio-calculator"
        ~children:[React.string "Fork me on GitHub"] () [@JSX];
      div ~style:Styles.controls ~children:[
        FlexRow.createElement ~children:[
          mk_label "tuning";
          Select.createElement ~spec:tuning_spec ~value:(Fretboard.Tuning.to_string tuning) () [@JSX]
        ] () [@JSX];
        FlexRow.createElement ~children:[
          mk_label "key";
          Select.createElement ~spec:root_pitch_spec ~value:(Note.to_string root_pitch_class) () [@JSX]
        ] () [@JSX];
        FlexRow.createElement ~children:[
          mk_label "accidental";
          Select.createElement ~spec:accidental_spec ~value:(Accidental.to_string accidental) () [@JSX]
        ] () [@JSX];
        FlexRow.createElement ~children:[
          mk_label "interval";
          Select.createElement ~spec:interval_type_spec
            ~value:(interval_type |> Stdlib.Option.map Interval.to_string |> Stdlib.Option.value ~default:"None") () [@JSX]
        ] () [@JSX];
        FlexRow.createElement ~children:[
          mk_label "chord";
          Select.createElement ~spec:chord_quality_spec
            ~value:(chord_quality |> Stdlib.Option.map Chord.quality_to_string |> Stdlib.Option.value ~default:"None") () [@JSX]
        ] () [@JSX];
        FlexRow.createElement ~children:[
          mk_label "scale";
          Select.createElement ~spec:scale_quality_spec
            ~value:(scale_quality |> Stdlib.Option.map Scale.quality_to_string |> Stdlib.Option.value ~default:"None") () [@JSX]
        ] () [@JSX];
        FlexRow.createElement ~children:[
          mk_label "progression";
          Select.createElement ~spec:progression_type_spec
            ~value:(progression_type |> Stdlib.Option.map string_of_progression_type |> Stdlib.Option.value ~default:"None") () [@JSX]
        ] () [@JSX];
      ] () [@JSX];
      harmonization;
      AnnotatedFretboard.createElement ~notes ~tuning ~key_signature () [@JSX];
      progression;
    ] () [@JSX]
  [@@react.component]
end;;

match ReactDOM.querySelector "#root" with
  | Some element ->
      let root = ReactDOM.Client.createRoot element in
      ReactDOM.Client.render root (App.createElement ~children:[] () [@JSX])
  | None ->
      Js.Console.error
        "Failed to start React: couldn't find the #root element"
