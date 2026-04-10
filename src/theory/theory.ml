open Belt

module Result_syntax = struct
  let ( let+ ) = Result.map
  let ( let* ) = Result.flatMap
end

let semitones_in_octave = 12

module Accidental = struct
  type t =
    | Double_flat
    | Flat
    | Natural
    | Sharp
    | Double_sharp

  let to_string = function
    | Double_flat -> {js|♭♭|js}
    | Flat -> {js|♭|js}
    | Natural -> ""
    | Sharp -> {js|♯|js}
    | Double_sharp -> {js|♯♯|js}

  let to_semitones = function
    | Double_flat -> -2
    | Flat -> -1
    | Natural -> 0
    | Sharp -> 1
    | Double_sharp -> 2

  let to_vexflow = function
    | Double_flat -> "bb"
    | Flat -> "b"
    | Natural -> ""
    | Sharp -> "#"
    | Double_sharp -> "##"
end

module Note = struct
  type pitch_class = C | D | E | F | G | A | B

  type t = { pitch_class : pitch_class; accidental : Accidental.t }

  let c accidental = { pitch_class = C; accidental }
  let d accidental = { pitch_class = D; accidental }
  let e accidental = { pitch_class = E; accidental }
  let f accidental = { pitch_class = F; accidental }
  let g accidental = { pitch_class = G; accidental }
  let a accidental = { pitch_class = A; accidental }
  let b accidental = { pitch_class = B; accidental }

  let set_accidental note accidental = { note with accidental }

  let is_same_pitch_class a b = a.pitch_class = b.pitch_class

  let pitch_class_to_string = function
    | C -> "C" | D -> "D" | E -> "E" | F -> "F"
    | G -> "G" | A -> "A" | B -> "B"

  let to_string note =
    pitch_class_to_string note.pitch_class ^ Accidental.to_string note.accidental

  let next_pitch_class = function
    | C -> D | D -> E | E -> F | F -> G
    | G -> A | A -> B | B -> C

  let get_next_note note =
    { pitch_class = next_pitch_class note.pitch_class; accidental = note.accidental }

  let pitch_class_to_semitones = function
    | C -> 0 | D -> 2 | E -> 4 | F -> 5
    | G -> 7 | A -> 9 | B -> 11

  let to_semitones note =
    pitch_class_to_semitones note.pitch_class + Accidental.to_semitones note.accidental

  let rec get_nth_note root n =
    match n with
    | 0 -> root
    | _ -> get_nth_note (get_next_note root) (n - 1)

  let semitones_between_notes a b =
    let delta = to_semitones b - to_semitones a in
    if delta < 0 then semitones_in_octave + delta else delta

  let to_vexflow_key note =
    let pc = match note.pitch_class with
      | C -> "C" | D -> "D" | E -> "E" | F -> "F"
      | G -> "G" | A -> "A" | B -> "B"
    in
    pc ^ Accidental.to_vexflow note.accidental

  let to_vexflow_note note octave =
    let pc = match note.pitch_class with
      | C -> "c" | D -> "d" | E -> "e" | F -> "f"
      | G -> "g" | A -> "a" | B -> "b"
    in
    pc ^ Accidental.to_vexflow note.accidental ^ "/" ^ string_of_int octave

  let to_tonejs_note note octave =
    let pc = match note.pitch_class with
      | C -> "C" | D -> "D" | E -> "E" | F -> "F"
      | G -> "G" | A -> "A" | B -> "B"
    in
    pc ^ Accidental.to_vexflow note.accidental ^ string_of_int octave

  let assign_octaves notes =
    match notes with
    | [] -> []
    | first :: rest ->
      let _, result = Stdlib.List.fold_left (fun (prev_semitones, acc) note ->
        let s = pitch_class_to_semitones note.pitch_class in
        let octave = if s <= prev_semitones then 5 else 4 in
        (s, (note, octave) :: acc)
      ) (pitch_class_to_semitones first.pitch_class, [(first, 4)]) rest
      in
      Stdlib.List.rev result
end


module Notes = struct
  type t = Note.t list

  let string_of_notes notes =
    notes |. List.map Note.to_string |> String.concat " "

  let has_note note notes = notes |. List.has note (fun a b -> a = b)

  let intersection notes_a notes_b =
    notes_a |. List.keep (fun note -> has_note note notes_b)

  let subtract notes_a notes_b =
    notes_a |. List.keep (fun note -> not (has_note note notes_b))

  let get_root = List.head
end


module Progression = struct
  type t = Note.t list list

  let to_string p =
    p |. List.map Notes.string_of_notes |> String.concat " | "
end

type semitone = int


(*
   for second, third, sixth and seventh:
     quality =
               ...
               -3 => doubly diminished
               -2 => diminished
               -1 => minor
               0 => major
               1 => augmented
               2 => doubly augmented
               ...

   for fourth and fifth:
     quality =
                ...
                -2 => doubly diminished
                -1 => diminished
                0 => perfect
                1 => augmented
                2 => doubly augmented
                ...
   quality: 0 is either major or perfect
*)


module Interval = struct
  module PerfectQuality = struct
    type t =
      | Diminished
      | Perfect
      | Augmented

    let to_semitones = function
      | Diminished -> -1
      | Perfect -> 0
      | Augmented -> 1

    let of_semitones = function
      | -1 -> Result.Ok Diminished
      | 0 -> Result.Ok Perfect
      | 1 -> Result.Ok Augmented
      | _ -> Result.Error "Semitones greater than 1 or smaller than -1 are not supported."

    let to_string = function
      | Diminished -> "d"
      | Perfect -> "P"
      | Augmented -> "A"
  end

  module MajorMinorQuality = struct
    type t =
      | Diminished
      | Minor
      | Major
      | Augmented

    let to_semitones = function
      | Diminished -> -2
      | Minor -> -1
      | Major -> 0
      | Augmented -> 1

    let of_semitones = function
      | -2 -> Result.Ok Diminished
      | -1 -> Result.Ok Minor
      | 0 -> Result.Ok Major
      | 1 -> Result.Ok Augmented
      | _ -> Result.Error "Semitones greater than 1 or smaller than -2 are not supported."

    let to_string = function
      | Diminished -> "d"
      | Minor -> "m"
      | Major -> "M"
      | Augmented -> "A"
  end

  type t =
    | Unison
    | Second of MajorMinorQuality.t
    | Third of MajorMinorQuality.t
    | Fourth of PerfectQuality.t
    | Fifth of PerfectQuality.t
    | Sixth of MajorMinorQuality.t
    | Seventh of MajorMinorQuality.t
    | Octave
    | Ninth of MajorMinorQuality.t
    | Eleventh of PerfectQuality.t
    | Thirteenth of MajorMinorQuality.t

  let to_semitones = function
    | Unison -> 0
    | Second q -> 2 + MajorMinorQuality.to_semitones q
    | Third q -> 4 + MajorMinorQuality.to_semitones q
    | Fourth q -> 5 + PerfectQuality.to_semitones q
    | Fifth q -> 7 + PerfectQuality.to_semitones q
    | Sixth q -> 9 + MajorMinorQuality.to_semitones q
    | Seventh q -> 11 + MajorMinorQuality.to_semitones q
    | Octave -> semitones_in_octave
    | Ninth q -> 14 + MajorMinorQuality.to_semitones q
    | Eleventh q -> 17 + PerfectQuality.to_semitones q
    | Thirteenth q -> 21 + MajorMinorQuality.to_semitones q

  let n_notes_of_interval = function
    | Unison -> 1
    | Second _ -> 2
    | Third _ -> 3
    | Fourth _ -> 4
    | Fifth _ -> 5
    | Sixth _ -> 6
    | Seventh _ -> 7
    | Octave -> 8
    | Ninth _ -> 9
    | Eleventh _ -> 11
    | Thirteenth _ -> 13

  (* Build an interval from note count + semitone count.
     Uses the reference semitones for each interval number to compute quality offset. *)
  let from_semitones n_notes n_semitones =
    let open Result_syntax in
    match n_notes with
    | 1 ->
      if n_semitones = 0 then Result.Ok Unison
      else Result.Error "semitones != 0 are not supported with Unison"
    | 2 ->
      let+ q = MajorMinorQuality.of_semitones (n_semitones - 2) in Second q
    | 3 ->
      let+ q = MajorMinorQuality.of_semitones (n_semitones - 4) in Third q
    | 4 ->
      let+ q = PerfectQuality.of_semitones (n_semitones - 5) in Fourth q
    | 5 ->
      let+ q = PerfectQuality.of_semitones (n_semitones - 7) in Fifth q
    | 6 ->
      let+ q = MajorMinorQuality.of_semitones (n_semitones - 9) in Sixth q
    | 7 ->
      let+ q = MajorMinorQuality.of_semitones (n_semitones - 11) in Seventh q
    | 8 ->
      if n_semitones = 12 then Result.Ok Octave
      else Result.Error "semitones != 12 are not supported with Octave"
    | 9 ->
      let+ q = MajorMinorQuality.of_semitones (n_semitones - 14) in Ninth q
    | 11 ->
      let+ q = PerfectQuality.of_semitones (n_semitones - 17) in Eleventh q
    | 13 ->
      let+ q = MajorMinorQuality.of_semitones (n_semitones - 21) in Thirteenth q
    | _ -> Result.Error ("unsupported interval with " ^ string_of_int n_notes ^ " notes")

  let to_string = function
    | Unison -> "P1"
    | Second q -> MajorMinorQuality.to_string q ^ "2"
    | Third q -> MajorMinorQuality.to_string q ^ "3"
    | Fourth q -> PerfectQuality.to_string q ^ "4"
    | Fifth q -> PerfectQuality.to_string q ^ "5"
    | Sixth q -> MajorMinorQuality.to_string q ^ "6"
    | Seventh q -> MajorMinorQuality.to_string q ^ "7"
    | Octave -> "P8"
    | Ninth q -> MajorMinorQuality.to_string q ^ "9"
    | Eleventh q -> PerfectQuality.to_string q ^ "11"
    | Thirteenth q -> MajorMinorQuality.to_string q ^ "13"

  let rec interval_number_of_notes note_a note_b acc =
    if Note.is_same_pitch_class note_a note_b then acc
    else interval_number_of_notes (Note.get_next_note note_a) note_b (acc + 1)

  let from_notes note_a note_b =
    let open Result_syntax in
    let delta = Note.semitones_between_notes note_a note_b in
    match interval_number_of_notes note_a note_b 0 with
    | 0 -> Result.Ok Unison
    | 1 -> let+ q = MajorMinorQuality.of_semitones (delta - 2) in Second q
    | 2 -> let+ q = MajorMinorQuality.of_semitones (delta - 4) in Third q
    | 3 -> let+ q = PerfectQuality.of_semitones (delta - 5) in Fourth q
    | 4 -> let+ q = PerfectQuality.of_semitones (delta - 7) in Fifth q
    | 5 -> let+ q = MajorMinorQuality.of_semitones (delta - 9) in Sixth q
    | 6 -> let+ q = MajorMinorQuality.of_semitones (delta - 11) in Seventh q
    | 7 -> Result.Ok Octave
    | _ -> Result.Error "n_notes > 8 (Octave) are not supported"

  let note_of_generic_interval root interval =
    Note.get_nth_note root (n_notes_of_interval interval - 1)

  let is_compound = function
    | Ninth _ | Eleventh _ | Thirteenth _ -> true
    | _ -> false

  let next_note root interval =
    let new_note = Note.set_accidental (note_of_generic_interval root interval) Accidental.Natural in
    let target = to_semitones interval in
    let actual_simple = Note.semitones_between_notes root new_note in
    let actual = if is_compound interval then actual_simple + 12 else actual_simple in
    let accidental = match target - actual with
      | -2 -> Accidental.Double_flat
      | -1 -> Accidental.Flat
      | 0 -> Accidental.Natural
      | 1 -> Accidental.Sharp
      | 2 -> Accidental.Double_sharp
      | _ -> Accidental.Natural
    in
    Note.set_accidental new_note accidental

  let to_notes root interval = [root; next_note root interval]

  let add_intervals a b =
    let n_notes = n_notes_of_interval a + n_notes_of_interval b - 1 in
    let n_semitones = to_semitones a + to_semitones b in
    from_semitones n_notes n_semitones

  let subtract_intervals a b =
    let n_notes = n_notes_of_interval a - n_notes_of_interval b + 1 in
    let n_semitones = to_semitones a - to_semitones b in
    from_semitones n_notes n_semitones
end


module Intervals = struct
  type t =
    | Relative of Interval.t list
    | Absolute of Interval.t list

  let rec stack_intervals_relatively root intervals =
    match intervals with
    | [] -> [root]
    | [interval] -> [root; Interval.next_note root interval]
    | interval :: rest ->
      let next = Interval.next_note root interval in
      let tail = match stack_intervals_relatively next rest with
        | [] | [_] -> []
        | _ :: rest -> rest
      in
      [root; next] |. List.concat tail

  let stack_intervals_absolutely root intervals =
    root :: List.map intervals (fun interval -> Interval.next_note root interval)

  let to_notes intervals root =
    match intervals with
    | Relative intervals -> stack_intervals_relatively root intervals
    | Absolute intervals -> stack_intervals_absolutely root intervals

  let relative_intervals_of_notes notes =
    let rec aux = function
      | root :: next :: rest ->
        root
        |. Interval.from_notes next
        |. Result.mapWithDefault [] (fun interval -> [interval])
        |. List.concat (aux (next :: rest))
      | [] | [_] -> []
    in
    Relative (aux notes)

  let absolute_intervals_of_notes notes =
    let rec aux = function
      | root :: next :: rest ->
        root
        |. Interval.from_notes next
        |. Result.mapWithDefault [] (fun interval -> [interval])
        |. List.concat (aux (root :: rest))
      | [] | [_] -> []
    in
    Absolute (aux notes)

  let to_list = function
    | Absolute intervals | Relative intervals -> intervals

  let to_string intervals =
    intervals |. to_list |. List.map Interval.to_string |> String.concat {js| · |js}

  let map intervals f =
    match intervals with
    | Absolute intervals -> Absolute (f intervals)
    | Relative intervals -> Relative (f intervals)

  let to_absolute relative_intervals =
    let open Result_syntax in
    let rec fold = function
      | [] | [_] -> Result.Ok []
      | first :: second :: rest ->
        let* combined = Interval.add_intervals first second in
        let+ tail = fold (combined :: rest) in
        combined :: tail
    in
    match relative_intervals with
    | Absolute _ as abs -> Result.Ok abs
    | Relative rel ->
      let+ abs = match rel with
        | [] | [_] -> Result.Ok rel
        | first :: _ ->
          let+ tail = fold rel in
          first :: tail
      in
      Absolute abs

  let to_relative absolute_intervals =
    let open Result_syntax in
    let rec fold = function
      | [] | [_] -> Result.Ok []
      | first :: second :: rest ->
        let* relative = Interval.subtract_intervals first second in
        let+ tail = fold (second :: rest) in
        relative :: tail
    in
    match absolute_intervals with
    | Relative _ as rel -> Result.Ok rel
    | Absolute abs ->
      let+ rel = match abs with
        | [] | [_] -> Result.Ok abs
        | first :: _ ->
          let+ tail = fold (abs |. List.reverse) in
          first :: List.reverse tail
      in
      Relative rel
end


module Chord = struct
  type quality =
    | Major_triad
    | Minor_triad
    | Augmented_triad
    | Diminished_triad
    | Suspended_triad
    | Power_chord
    | Diminished_power_chord
    | Augmented_power_chord
    | Major_seventh
    | Dominant_seventh
    | Minor_seventh_major
    | Minor_seventh
    | Augmented_major_seventh
    | Half_diminished_seventh
    | Diminished_seventh
    | Suspended_seventh
    | Seventh_augmented_fifth
    | Seventh_diminished_fifth
    | Major_sixth
    | Minor_sixth
    | Major_ninth
    | Dominant_ninth
    | Minor_ninth
    | Minor_major_ninth
    | Dominant_eleventh
    | Minor_eleventh
    | Dominant_thirteenth
    | Minor_thirteenth
    | Major_six_nine
    | Minor_six_nine

  type t = { root : Note.t; quality : quality }

  let make root quality = { root; quality }

  let quality_to_string = function
    | Major_triad -> "maj"
    | Minor_triad -> "m"
    | Augmented_triad -> "+"
    | Diminished_triad -> {js|°|js}
    | Suspended_triad -> "sus4"
    | Power_chord -> "5"
    | Diminished_power_chord -> {js|♭5|js}
    | Augmented_power_chord -> {js|♯5|js}
    | Major_seventh -> {js|△7|js}
    | Dominant_seventh -> "7"
    | Minor_seventh_major -> {js|m△7|js}
    | Minor_seventh -> "m7"
    | Augmented_major_seventh -> {js|+△7|js}
    | Half_diminished_seventh -> {js|ø7|js}
    | Diminished_seventh -> {js|°7|js}
    | Suspended_seventh -> "7sus4"
    | Seventh_augmented_fifth -> {js|7♯5|js}
    | Seventh_diminished_fifth -> {js|7♭5|js}
    | Major_sixth -> "6"
    | Minor_sixth -> "m6"
    | Major_ninth -> {js|△9|js}
    | Dominant_ninth -> "9"
    | Minor_ninth -> "m9"
    | Minor_major_ninth -> {js|m△9|js}
    | Dominant_eleventh -> "11"
    | Minor_eleventh -> "m11"
    | Dominant_thirteenth -> "13"
    | Minor_thirteenth -> "m13"
    | Major_six_nine -> "6/9"
    | Minor_six_nine -> "m6/9"

  let to_string chord =
    Note.to_string chord.root ^ " " ^ quality_to_string chord.quality

  let to_intervals quality =
    let open Interval in
    match quality with
    | Major_triad -> Intervals.Relative [Major |. Third; Minor |. Third]
    | Minor_triad -> Intervals.Relative [Minor |. Third; Major |. Third]
    | Augmented_triad -> Intervals.Relative [Major |. Third; Major |. Third]
    | Diminished_triad -> Intervals.Relative [Minor |. Third; Minor |. Third]
    | Suspended_triad -> Intervals.Relative [Perfect |. Fourth; Major |. Second]
    | Power_chord -> Intervals.Relative [Perfect |. Fifth]
    | Augmented_power_chord -> Intervals.Relative [Augmented |. Fifth]
    | Diminished_power_chord -> Intervals.Relative [Diminished |. Fifth]
    | Major_seventh -> Intervals.Relative [Major |. Third; Minor |. Third; Major |. Third]
    | Dominant_seventh -> Intervals.Relative [Major |. Third; Minor |. Third; Minor |. Third]
    | Minor_seventh_major -> Intervals.Relative [Minor |. Third; Major |. Third; Major |. Third]
    | Minor_seventh -> Intervals.Relative [Minor |. Third; Major |. Third; Minor |. Third]
    | Augmented_major_seventh -> Intervals.Relative [Major |. Third; Major |. Third; Minor |. Third]
    | Half_diminished_seventh ->
      Intervals.Absolute [Minor |. Third; Diminished |. Fifth; Minor |. Seventh]
    | Diminished_seventh ->
      Intervals.Absolute [Minor |. Third; Diminished |. Fifth; Diminished |. Seventh]
    | Suspended_seventh ->
      Intervals.Absolute [Perfect |. Fourth; Perfect |. Fifth; Minor |. Seventh]
    | Seventh_augmented_fifth ->
      Intervals.Absolute [Major |. Third; Augmented |. Fifth; Minor |. Seventh]
    | Seventh_diminished_fifth ->
      Intervals.Absolute [Major |. Third; Diminished |. Fifth; Minor |. Seventh]
    | Major_sixth -> Intervals.Absolute [Major |. Third; Perfect |. Fifth; Major |. Sixth]
    | Minor_sixth -> Intervals.Absolute [Minor |. Third; Perfect |. Fifth; Major |. Sixth]
    | Major_ninth ->
      Intervals.Absolute [Major |. Third; Perfect |. Fifth; Major |. Seventh; Major |. Ninth]
    | Dominant_ninth ->
      Intervals.Absolute [Major |. Third; Perfect |. Fifth; Minor |. Seventh; Major |. Ninth]
    | Minor_ninth ->
      Intervals.Absolute [Minor |. Third; Perfect |. Fifth; Minor |. Seventh; Major |. Ninth]
    | Minor_major_ninth ->
      Intervals.Absolute [Minor |. Third; Perfect |. Fifth; Major |. Seventh; Major |. Ninth]
    | Dominant_eleventh ->
      Intervals.Absolute [Major |. Third; Perfect |. Fifth; Minor |. Seventh; Major |. Ninth; Perfect |. Eleventh]
    | Minor_eleventh ->
      Intervals.Absolute [Minor |. Third; Perfect |. Fifth; Minor |. Seventh; Major |. Ninth; Perfect |. Eleventh]
    | Dominant_thirteenth ->
      Intervals.Absolute [Major |. Third; Perfect |. Fifth; Minor |. Seventh; Major |. Ninth; Major |. Thirteenth]
    | Minor_thirteenth ->
      Intervals.Absolute [Minor |. Third; Perfect |. Fifth; Minor |. Seventh; Major |. Ninth; Major |. Thirteenth]
    | Major_six_nine ->
      Intervals.Absolute [Major |. Third; Perfect |. Fifth; Major |. Sixth; Major |. Ninth]
    | Minor_six_nine ->
      Intervals.Absolute [Minor |. Third; Perfect |. Fifth; Major |. Sixth; Major |. Ninth]

  let rec quality_of_intervals intervals =
    let open Result_syntax in
    let open Interval in
    match intervals with
    | Intervals.Relative _ ->
      let* abs = Intervals.to_absolute intervals in
      quality_of_intervals abs
    | Intervals.Absolute abs ->
      match abs with
      | [Third Minor; Fifth Perfect] -> Result.Ok Minor_triad
      | [Third Major; Fifth Perfect] -> Result.Ok Major_triad
      | [Third Minor; Fifth Diminished] -> Result.Ok Diminished_triad
      | [Third Major; Fifth Augmented] -> Result.Ok Augmented_triad
      | [Third Minor; Fifth Perfect; Seventh Minor] -> Result.Ok Minor_seventh
      | [Third Major; Fifth Perfect; Seventh Major] -> Result.Ok Major_seventh
      | [Third Minor; Fifth Diminished; Seventh Minor] -> Result.Ok Half_diminished_seventh
      | [Third Major; Fifth Perfect; Seventh Minor] -> Result.Ok Dominant_seventh
      | [Third Minor; Fifth Diminished; Seventh Diminished] -> Result.Ok Diminished_seventh
      | [Third Major; Fifth Augmented; Seventh Major] -> Result.Ok Augmented_major_seventh
      | [Third Minor; Fifth Perfect; Seventh Major] -> Result.Ok Minor_seventh_major
      | [Third Major; Fifth Perfect; Seventh Major; Ninth Major] -> Result.Ok Major_ninth
      | [Third Major; Fifth Perfect; Seventh Minor; Ninth Major] -> Result.Ok Dominant_ninth
      | [Third Minor; Fifth Perfect; Seventh Minor; Ninth Major] -> Result.Ok Minor_ninth
      | [Third Minor; Fifth Perfect; Seventh Major; Ninth Major] -> Result.Ok Minor_major_ninth
      | [Third Major; Fifth Perfect; Seventh Minor; Ninth Major; Eleventh Perfect] -> Result.Ok Dominant_eleventh
      | [Third Minor; Fifth Perfect; Seventh Minor; Ninth Major; Eleventh Perfect] -> Result.Ok Minor_eleventh
      | [Third Major; Fifth Perfect; Seventh Minor; Ninth Major; Thirteenth Major] -> Result.Ok Dominant_thirteenth
      | [Third Minor; Fifth Perfect; Seventh Minor; Ninth Major; Thirteenth Major] -> Result.Ok Minor_thirteenth
      | [Third Major; Fifth Perfect; Sixth Major; Ninth Major] -> Result.Ok Major_six_nine
      | [Third Minor; Fifth Perfect; Sixth Major; Ninth Major] -> Result.Ok Minor_six_nine
      | _ -> Result.Error "Could not find matching chord"

  let from_intervals root intervals =
    let open Result_syntax in
    let+ quality = quality_of_intervals intervals in
    make root quality

  let to_notes root quality =
    to_intervals quality |. Intervals.to_notes root
end


module Chords = struct
  type t = Chord.t list

  let to_string chords =
    chords |. List.map Chord.to_string |> String.concat " | "
end


module Scale = struct
  type quality =
    | Major
    | Natural_minor
    | Harmonic_minor
    | Ionian
    | Dorian
    | Phrygian
    | Lydian
    | Mixolydian
    | Aeolian
    | Locrian

  type t = { root : Note.t; quality : quality }

  let make root quality = { root; quality }

  let quality_to_string = function
    | Major -> "Major Scale"
    | Natural_minor -> "Natural Minor"
    | Harmonic_minor -> "Harmonic Minor"
    | Ionian -> "Ionian Mode"
    | Dorian -> "Dorian Mode"
    | Phrygian -> "Phrygian Mode"
    | Lydian -> "Lydian Mode"
    | Mixolydian -> "Mixolydian Mode"
    | Aeolian -> "Aeolian Mode"
    | Locrian -> "Locrian Mode"

  let to_string scale =
    Note.to_string scale.root ^ " " ^ quality_to_string scale.quality

  let rec get_nth_mode intervals n =
    match n with
    | 0 -> intervals
    | _ ->
      match intervals with
      | Intervals.Relative ([] | [_]) -> intervals
      | Intervals.Relative (head :: tail) ->
        get_nth_mode (Intervals.Relative (List.concat tail [head])) (n - 1)
      | Intervals.Absolute _ -> Intervals.Relative []

  let major_intervals =
    let open Interval in
    Intervals.Relative [
      Major |. Second;
      Major |. Second;
      Minor |. Second;
      Major |. Second;
      Major |. Second;
      Major |. Second;
      Minor |. Second;
    ]

  let rec to_intervals quality =
    let open Interval in
    match quality with
    | Major -> major_intervals
    | Ionian -> get_nth_mode (to_intervals Major) 0
    | Dorian -> get_nth_mode (to_intervals Major) 1
    | Phrygian -> get_nth_mode (to_intervals Major) 2
    | Lydian -> get_nth_mode (to_intervals Major) 3
    | Mixolydian -> get_nth_mode (to_intervals Major) 4
    | Aeolian -> get_nth_mode (to_intervals Major) 5
    | Locrian -> get_nth_mode (to_intervals Major) 6
    | Natural_minor ->
      Intervals.Relative [
        Major |. Second;
        Minor |. Second;
        Major |. Second;
        Major |. Second;
        Minor |. Second;
        Major |. Second;
        Major |. Second;
      ]
    | Harmonic_minor ->
      Intervals.Relative [
        Major |. Second;
        Minor |. Second;
        Major |. Second;
        Major |. Second;
        Minor |. Second;
        Augmented |. Second;
        Minor |. Second;
      ]

  let to_notes root quality =
    to_intervals quality |. Intervals.to_notes root

  let string_of_intervals intervals =
    intervals |. List.map Interval.to_string |> String.concat " -> "

  (* Map scale quality + root to VexFlow key signature string.
     For modes, derive the parent major key. *)
  let to_vexflow_key root quality =
    let open Interval in
    match quality with
    | Major | Ionian -> Note.to_vexflow_key root
    | Dorian -> Note.to_vexflow_key (next_note root (Minor |. Seventh))
    | Phrygian -> Note.to_vexflow_key (next_note root (Minor |. Sixth))
    | Lydian -> Note.to_vexflow_key (next_note root (Perfect |. Fifth))
    | Mixolydian -> Note.to_vexflow_key (next_note root (Perfect |. Fourth))
    | Aeolian | Natural_minor -> Note.to_vexflow_key root ^ "m"
    | Locrian -> Note.to_vexflow_key (next_note root (Minor |. Second))
    | Harmonic_minor -> Note.to_vexflow_key root ^ "m"
end


module Harmonization = struct
  open Result_syntax

  let rec result_all = function
    | [] -> Result.Ok []
    | Result.Error e :: _ -> Result.Error ("Matrix has an Error element: " ^ e)
    | Result.Ok x :: xs ->
      let+ rest = result_all xs in
      x :: rest

  (* see https://www.bluesguitarinstitute.com/how-to-harmonize-a-scale/ *)
  let get_harmonization_matrix root quality =
    let intervals = Scale.to_intervals quality in
    let intervals_matrix =
      intervals
      |. Intervals.to_list
      |. List.mapWithIndex (fun i _ -> Scale.get_nth_mode intervals i)
    in
    let* first_row_intervals =
      intervals_matrix |. List.head
      |> Stdlib.Option.to_result ~none:"empty matrix"
    in
    let first_row_notes = first_row_intervals |. Intervals.to_notes root in
    intervals_matrix
    |. List.mapWithIndex (fun i intervals ->
        let+ row_root =
          first_row_notes |. List.get i
          |> Stdlib.Option.to_result ~none:"no square matrix"
        in
        intervals |. Intervals.to_notes row_root
      )
    |. result_all

  let print_harmonization_matrix matrix =
    matrix |. List.map Notes.string_of_notes |> String.concat "\n"

  let harmonize_scale root quality =
    let* matrix = get_harmonization_matrix root quality in
    matrix
    |. List.map (fun notes ->
        notes |. List.keepWithIndex (fun _note index ->
          List.has [1; 3; 5; 7] (index + 1) (=)))
    |. List.map (fun notes ->
        (List.headExn notes, Intervals.relative_intervals_of_notes notes))
    |. List.map (fun (root, intervals) -> Chord.from_intervals root intervals)
    |. result_all

  let rec transpose = function
    | [] -> []
    | ([] :: _) -> []
    | rows ->
      let heads = rows |. List.map List.headExn in
      let tails = rows |. List.map List.tailExn in
      heads :: transpose tails

  let transpose_harmonization_matrix matrix =
    matrix
    |. List.reduceReverse (Result.Ok []) (fun acc intervals ->
        match intervals with
        | Intervals.Absolute _ -> Result.Error "Only relative intervals are supported"
        | Intervals.Relative intervals ->
          let+ acc = acc in
          intervals :: acc
      )
    |. Result.map (fun matrix ->
        matrix |> transpose |. List.map (fun intervals -> Intervals.Relative intervals))

  let filter_list_by_indexes notes indexes =
    notes |. List.keepWithIndex (fun _note i -> Set.Int.has indexes i)

  let filter_and_maintain_order l indexes =
    indexes |. Array.reduceReverse (Result.Ok []) (fun acc scale_degree ->
      let* acc = acc in
      match l |. List.get scale_degree with
      | Some chord -> Result.Ok (chord :: acc)
      | None -> Result.Error "scale degree not found"
    )

  let to_chords quality chord_degrees scale_degrees =
    quality
    |> Scale.to_intervals
    |. Intervals.to_list
    |. List.mapWithIndex (fun i _ -> Scale.get_nth_mode (Scale.to_intervals quality) i)
    |> transpose_harmonization_matrix
    |. Result.flatMap (fun matrix ->
        filter_and_maintain_order matrix scale_degrees
        |. Result.flatMap (fun chords ->
            chords |. List.reduceReverse (Result.Ok []) (fun acc scale_intervals ->
              let intervals =
                scale_intervals
                |. Intervals.to_absolute
                |. Result.map (fun intervals ->
                    Intervals.map intervals (fun intervals ->
                      filter_list_by_indexes intervals chord_degrees))
              in
              match intervals |. Result.flatMap (Chord.quality_of_intervals) with
              | Result.Ok quality ->
                let+ acc = acc in
                quality :: acc
              | Result.Error msg -> Result.Error msg
            )
          )
      )

  let to_progression quality root chord_degrees scale_degrees =
    let scale_notes =
      filter_and_maintain_order (Scale.to_notes root quality) scale_degrees
    in
    let scale_chords = to_chords quality chord_degrees scale_degrees in
    match (scale_notes, scale_chords) with
    | (Result.Ok notes, Result.Ok chords) ->
      Result.Ok (
        List.zip notes chords
        |. List.map (fun (root, quality) -> Chord.to_notes root quality)
      )
    | (Result.Error msg, _) | (_, Result.Error msg) -> Result.Error msg

  let triad_degrees = [|1; 3|] |. Set.Int.fromArray
  let tetrad_degrees = [|1; 3; 5|] |. Set.Int.fromArray
  let scale_harmonization_degrees = [|0; 1; 2; 3; 4; 5; 6|]

  let to_triads quality = to_chords quality triad_degrees scale_harmonization_degrees
  let to_tetrads quality = to_chords quality tetrad_degrees scale_harmonization_degrees

  let to_triad_progression quality root =
    to_progression quality root triad_degrees scale_harmonization_degrees
  let to_tetrad_progression quality root =
    to_progression quality root tetrad_degrees scale_harmonization_degrees
end
