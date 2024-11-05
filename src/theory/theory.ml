open Belt


let ( let+ ) = Result.map
let ( let* ) = Result.flatMap

let semitones_in_octave = 12

module Accidental = struct
  type t =
    | DoubleFlat
    | Flat
    | Natural
    | Sharp
    | DoubleSharp

  let to_string accidental =
    match accidental with
    | Flat -> "b"
    | DoubleFlat -> "bb"
    | Natural -> ""
    | DoubleSharp -> "##"
    | Sharp -> "#"

  let to_semitones accidental =
    match accidental with
    | DoubleFlat -> -2
    | Flat -> -1
    | Natural -> 0
    | Sharp -> 1
    | DoubleSharp -> 2
end;;

module Note = struct
  open Accidental

  type t =
    | C of Accidental.t
    | D of Accidental.t
    | E of Accidental.t
    | F of Accidental.t
    | G of Accidental.t
    | A of Accidental.t
    | B of Accidental.t

  let set_accidental note accidental =
    match note with
    | C(_) -> C(accidental)
    | D(_) -> D(accidental)
    | E(_) -> E(accidental)
    | F(_) -> F(accidental)
    | G(_) -> G(accidental)
    | A(_) -> A(accidental)
    | B(_) -> B(accidental)

  let is_same_note_familly noteA noteB =
    match (noteA, noteB) with
    | (C(_), C(_))
    | (D(_), D(_))
    | (E(_), E(_))
    | (F(_), F(_))
    | (G(_), G(_))
    | (A(_), A(_))
    | (B(_), B(_)) -> true
    | (_, _) -> false

  let to_string note =
    match note with
    | C(accidental) -> "C" ^ (accidental |> to_string)
    | D(accidental) -> "D" ^ (accidental |> to_string)
    | E(accidental) -> "E" ^ (accidental |> to_string)
    | F(accidental) -> "F" ^ (accidental |> to_string)
    | G(accidental) -> "G" ^ (accidental |> to_string)
    | A(accidental) -> "A" ^ (accidental |> to_string)
    | B(accidental) -> "B" ^ (accidental |> to_string)

  let get_next_note note =
    match note with
    | C(accidental) -> D(accidental)
    | D(accidental) -> E(accidental)
    | E(accidental) -> F(accidental)
    | F(accidental) -> G(accidental)
    | G(accidental) -> A(accidental)
    | A(accidental) -> B(accidental)
    | B(accidental) -> C(accidental)

  let to_semitones note =
    match note with
    | C(accidental) -> 0 + (accidental |> to_semitones)
    | D(accidental) -> 2 + (accidental |> to_semitones)
    | E(accidental) -> 4 + (accidental |> to_semitones)
    | F(accidental) -> 5 + (accidental |> to_semitones)
    | G(accidental) -> 7 + (accidental |> to_semitones)
    | A(accidental) -> 9 + (accidental |> to_semitones)
    | B(accidental) -> 11 + (accidental |> to_semitones)

  let rec get_nth_note rootNote n =
    match n with
    | 0 -> rootNote
    | _ -> get_nth_note (rootNote |> get_next_note)  (n - 1)

  let semitones_between_notes noteA noteB =
    let delta = (noteB |> to_semitones) - (noteA |> to_semitones) in
    match delta < 0 with
    | true -> semitones_in_octave + delta
    | false -> delta
end;;


module Notes = struct
  type t = Note.t List.t

  let string_of_notes notes =
    notes
    |. List.map Note.to_string
    |> String.concat " "

  let has_note note notes = notes |. List.has note (fun a b -> a = b)

  let intersection notesA notesB =
    notesA |. List.keep (fun note -> has_note note notesB)

  let subtract notesA notesB =
    notesA |. List.keep (fun note -> not (has_note note notesB))

  let get_tonic = List.head
end;;


module Progression = struct
  type t = Note.t List.t List.t

  let to_string p =
    p
    |. List.map Notes.string_of_notes
    |> String.concat " | "
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

    module Errors = struct
      let semitones = Result.Error "Semitones greater that 1 or smaller that -1 are not supported."
    end

    let to_semitones qualifier =
      match qualifier with
      | Diminished -> -1
      | Perfect -> 0
      | Augmented -> 1

    let quality_of_semitones semitones =
      match semitones with
      | -1 -> Result.Ok(Diminished)
      | 0 -> Result.Ok(Perfect)
      | 1 -> Result.Ok(Augmented)
      | _ -> Errors.semitones

    let to_string qualifier =
      match qualifier with
      | Diminished -> "diminished"
      | Perfect -> "perfect"
      | Augmented -> "augmented"
  end

  module MajorMinorQuality = struct
    type t =
      | Diminished
      | Minor
      | Major
      | Augmented

    module Errors = struct
      let semitones = Result.Error "Semitones greater that 1 or smaller that -2 are not supported."
    end

    let to_semitones qualifier =
      match qualifier with
      | Diminished -> -2
      | Minor -> -1
      | Major -> 0
      | Augmented -> 1

    let quality_of_semitones semitones =
      match semitones with
      | -2 -> Result.Ok(Diminished)
      | -1 -> Result.Ok(Minor)
      | 0 -> Result.Ok(Major)
      | 1 -> Result.Ok(Augmented)
      | _ -> Errors.semitones

    let to_string qualifier =
      match qualifier with
      | Diminished -> "diminished"
      | Minor -> "minor"
      | Major -> "major"
      | Augmented -> "augmented"
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

  module Errors = struct
    let unison_semitones = Result.Error "semitones != 1 are not supported with Unison"
    let octave_semitones = Result.Error "semitones != 12 are not supported with Octave"
    let n_notes_too_large = Result.Error "nNotes > 8 (Octave) are not supported"
  end

  let to_semitones = function
    | Unison ->  Note.to_semitones(C(Natural))
    | Second(qualifier) ->  Note.to_semitones(D(Natural)) + MajorMinorQuality.to_semitones qualifier
    | Third(qualifier) ->  Note.to_semitones(E(Natural)) + MajorMinorQuality.to_semitones qualifier
    | Fourth(qualifier) ->  Note.to_semitones(F(Natural)) + PerfectQuality.to_semitones qualifier
    | Fifth(qualifier) ->  Note.to_semitones(G(Natural)) + PerfectQuality.to_semitones qualifier
    | Sixth(qualifier) ->  Note.to_semitones(A(Natural)) + MajorMinorQuality.to_semitones qualifier
    | Seventh(qualifier) ->  Note.to_semitones(B(Natural)) + MajorMinorQuality.to_semitones qualifier
    | Octave -> semitones_in_octave

  let n_notes_of_interval = function
    | Unison -> 1
    | Second _ -> 2
    | Third _ -> 3
    | Fourth _ -> 4
    | Fifth _ -> 5
    | Sixth _ -> 6
    | Seventh _ -> 7
    | Octave -> 8

  let from_semitones n_notes n_semitones =
    match n_notes with
    | 1 ->
      if n_semitones = 0 then Result.Ok(Unison)
      else Errors.unison_semitones
    | 2 ->
      let+ quality = (n_semitones - (Major |. Second |. to_semitones)) |> MajorMinorQuality.quality_of_semitones in
      Second(quality)
    | 3 ->
      let+ quality = (n_semitones - (Major |. Third |. to_semitones)) |> MajorMinorQuality.quality_of_semitones in
      Third(quality)
    | 4 ->
      let+ quality = (n_semitones - (Perfect |. Fourth |. to_semitones)) |> PerfectQuality.quality_of_semitones in
      Fourth(quality)
    | 5 ->
      let+ quality = (n_semitones - (Perfect |. Fifth |. to_semitones)) |> PerfectQuality.quality_of_semitones in
      Fifth(quality)
    | 6 ->
      let+ quality = (n_semitones - (Major |. Sixth |. to_semitones)) |> MajorMinorQuality.quality_of_semitones in
      Sixth(quality)
    | 7 ->
      let+ quality = (n_semitones - (Major |. Seventh |. to_semitones)) |> MajorMinorQuality.quality_of_semitones in
      Seventh(quality)
    | 8 ->
      if n_semitones = 12 then Result.Ok(Octave)
      else Errors.octave_semitones
    | _ -> Errors.n_notes_too_large

  let to_string = function
    | Unison -> "unison"
    | Second(qualifier) -> MajorMinorQuality.to_string(qualifier) ^ " second"
    | Third(qualifier) -> MajorMinorQuality.to_string(qualifier) ^ " third"
    | Fourth(qualifier) -> PerfectQuality.to_string(qualifier) ^ " fourth"
    | Fifth(qualifier) -> PerfectQuality.to_string(qualifier) ^ " fifth"
    | Sixth(qualifier) -> MajorMinorQuality.to_string(qualifier) ^ " sixth"
    | Seventh(qualifier) -> MajorMinorQuality.to_string(qualifier) ^ " seventh"
    | Octave -> "octave"

  let rec interval_number_of_notes note_a note_b distance_accumulator =
    if note_b |> Note.is_same_note_familly(note_a) then
      distance_accumulator
    else
      let next_note = note_a |> Note.get_next_note in
      interval_number_of_notes next_note note_b (distance_accumulator + 1)

  let from_notes note_a  note_b =
    let delta_semitones = Note.semitones_between_notes note_a note_b in
    match interval_number_of_notes note_a note_b 0 with
    | 0 -> Result.Ok(Unison)
    | 1 ->
      let semitones = delta_semitones - to_semitones(Major |. Second) in
      let+ quality = MajorMinorQuality.quality_of_semitones semitones in
      Second(quality)
    | 2 ->
      let semitones = delta_semitones - to_semitones(Major |. Third) in
      let+ quality = MajorMinorQuality.quality_of_semitones semitones in
      Third(quality)
    | 3 ->
      let semitones = delta_semitones - to_semitones(Perfect |. Fourth) in
      let+ quality = PerfectQuality.quality_of_semitones semitones in
      Fourth(quality)
    | 4 ->
      let semitones = delta_semitones - to_semitones(Perfect |. Fifth) in
      let+ quality = PerfectQuality.quality_of_semitones semitones in
      Fifth(quality)
    | 5 ->
      let semitones = delta_semitones - to_semitones(Major |. Sixth) in
      let+ quality = MajorMinorQuality.quality_of_semitones semitones in
      Sixth(quality)
    | 6 ->
      let semitones = delta_semitones - to_semitones(Major |. Seventh) in
      let+ quality = MajorMinorQuality.quality_of_semitones semitones in
      Seventh(quality)
    | 7 -> Result.Ok(Octave)
    | _ -> Errors.n_notes_too_large

  let note_of_cannonical_interval root_note interval =
    match interval with
    | Unison -> root_note
    | Second(_) -> root_note |. Note.get_nth_note(1)
    | Third(_) -> root_note |. Note.get_nth_note(2)
    | Fourth(_) -> root_note |. Note.get_nth_note(3)
    | Fifth(_) -> root_note |. Note.get_nth_note(4)
    | Sixth(_) -> root_note |. Note.get_nth_note(5)
    | Seventh(_) -> root_note |. Note.get_nth_note(6)
    | Octave -> root_note |. Note.get_nth_note(7)

  let next_note root_note interval =
    let new_note = root_note |. note_of_cannonical_interval(interval) |. Note.set_accidental(Natural) in
    let target_semiton_difference = interval |> to_semitones in
    let actual_semiton_difference = Note.semitones_between_notes root_note new_note in
    let accidental =
      match target_semiton_difference - actual_semiton_difference with
      | -2 -> Accidental.DoubleFlat
      | -1 -> Accidental.Flat
      | 0 -> Accidental.Natural
      | 1 -> Accidental.Sharp
      | 2 -> Accidental.DoubleSharp
      | _ -> Accidental.Natural
    in
    new_note |. Note.set_accidental(accidental)

  let to_notes root_note interval = [root_note; root_note |. next_note(interval)]

  let add_intervals interval_a interval_b =
    let n_notes = n_notes_of_interval interval_a + n_notes_of_interval interval_b - 1 in
    let n_semitones = to_semitones interval_a + to_semitones interval_b in
    from_semitones n_notes n_semitones

  let subtract_intervals interval_a interval_b =
    let n_notes = n_notes_of_interval interval_a - n_notes_of_interval interval_b + 1 in
    let n_semitones = to_semitones interval_a - to_semitones interval_b in
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
      let next_note = Interval.next_note root interval in
      let sub_chord =
        match stack_intervals_relatively next_note rest with
        | []
        | [_] -> []
        | _ :: rest -> rest
      in
      [root; next_note] |. List.concat(sub_chord)

  let stack_intervals_absolutely root intervals =
    let notes = List.map intervals (fun interval -> Interval.next_note root interval) in
    root :: notes

  let to_notes root intervals =
    match intervals with
    | Relative(intervals) -> stack_intervals_relatively root intervals
    | Absolute(intervals) -> stack_intervals_absolutely root intervals

  let relative_intervals_of_notes notes =
    let rec relative_intervals_of_notes notes =
      match notes with
      | root :: next_note :: rest ->
        root
        |. Interval.from_notes(next_note)
        |. Result.mapWithDefault [] (fun interval -> [interval])
        |. List.concat(next_note :: rest |. relative_intervals_of_notes)
      | []
      | [_] -> []
    in
    Relative(relative_intervals_of_notes notes)

  let absolute_intervals_of_notes notes =
    let rec absolute_intervals_of_notes notes =
      match notes with
      | root :: next_note :: rest ->
        root
        |. Interval.from_notes(next_note)
        |. Result.mapWithDefault [] (fun interval -> [interval])
        |. List.concat(root :: rest |. absolute_intervals_of_notes)
      | []
      | [_] -> []
    in
    Absolute(absolute_intervals_of_notes notes)

  let to_list = function
    | Absolute(intervals)
    | Relative(intervals) -> intervals

  let to_string intervals =
    intervals
    |. to_list
    |. List.map(fun i -> Interval.to_string i)
    |> String.concat " | "

  let map intervals f =
    match intervals with
    | Absolute(intervals) -> Absolute(f intervals)
    | Relative(intervals) -> Relative(f intervals)

  let to_absolute relative_intervals =
    let rec f relative_intervals =
      match relative_intervals with
      | [] -> Result.Ok([])
      | [_] -> Result.Ok([])
      | first :: second :: rest ->
        let* absolute_interval = Interval.add_intervals first second in
        let+ absolute_intervals = f (absolute_interval :: rest) in
        absolute_interval :: absolute_intervals
    in
    match relative_intervals with
    | Relative(relative_intervals) ->
      let+ absolute_intervals =
        match relative_intervals with
        | []
        | [_] -> Result.Ok relative_intervals
        | first :: _ ->
          let+ intervals = f relative_intervals in
          first :: intervals
      in
      Absolute(absolute_intervals)
    | Absolute(intervals) -> Result.Ok(Absolute(intervals))

  let to_relative absolute_intervals =
    let rec f reversed_absolute_intervals =
      match reversed_absolute_intervals with
      | [] -> Result.Ok([])
      | [_] -> Result.Ok([])
      | first :: second :: rest ->
        let* relative_interval = Interval.subtract_intervals first second in
        let+ relative_intervals = f (second :: rest) in
        relative_interval :: relative_intervals
    in
    match absolute_intervals with
    | Relative(intervals) -> Result.Ok(Relative(intervals))
    | Absolute(absolute_intervals) ->
      let+ relative_intervals =
        match absolute_intervals with
        | []
        | [_] -> Result.Ok absolute_intervals
        | first :: _ ->
          let+ intervals = f (absolute_intervals |. List.reverse) in
          first :: List.reverse intervals
      in
      Relative(relative_intervals)
end


module Chord = struct
  open Interval
  open Intervals

  type t =
    | MajorTriad of Note.t
    | MinorTriad of Note.t
    | AugmentedTriad of Note.t
    | DiminishedTriad of Note.t
    | SuspendedTriad of Note.t
    | PowerChord of Note.t
    | DiminishedPowerChord of Note.t
    | AugmentedPowerChord of Note.t
    | MajorSeventh of Note.t
    | DominanteSeventh of Note.t
    | MinorSeventhMajor of Note.t
    | MinorSeventh of Note.t
    | AugmentedMajorSeventh of Note.t
    | HalfDiminishedSeventh of Note.t
    | DiminishedSeventh of Note.t
    | SuspendedSeventh of Note.t
    | SeventhAugmentedFifth of Note.t
    | SeventhDiminishedFifth of Note.t
    | MajorSixth of Note.t
    | MinorSixth of Note.t

  let to_string chord =
    match chord with
    | MajorTriad(root) -> Note.to_string root ^ " majorTriad"
    | MinorTriad(root) -> Note.to_string root ^ " minorTriad"
    | AugmentedTriad(root) -> Note.to_string root ^ " augmentedTriad"
    | DiminishedTriad(root) -> Note.to_string root ^ " diminishedTriad"
    | SuspendedTriad(root) -> Note.to_string root ^ " suspendedTriad"
    | PowerChord(root) -> Note.to_string root ^ " powerChord"
    | DiminishedPowerChord(root) -> Note.to_string root ^ " diminishedPowerChord"
    | AugmentedPowerChord(root) -> Note.to_string root ^ " augmentedPowerChord"
    | MajorSeventh(root) -> Note.to_string root ^ " majorSeventh"
    | DominanteSeventh(root) -> Note.to_string root ^ " dominantSeventh"
    | MinorSeventhMajor(root) -> Note.to_string root ^ " minorSeventhMajor"
    | MinorSeventh(root) -> Note.to_string root ^ " minorSeventh"
    | AugmentedMajorSeventh(root) -> Note.to_string root ^ " augmentedMajorSeventh"
    | HalfDiminishedSeventh(root) -> Note.to_string root ^ " halfDiminishedSeventh"
    | DiminishedSeventh(root) -> Note.to_string root ^ " diminishedSeventh"
    | SuspendedSeventh(root) -> Note.to_string root ^ " suspendedSeventh"
    | SeventhAugmentedFifth(root) -> Note.to_string root ^ " seventhAugmentedFifth"
    | SeventhDiminishedFifth(root) -> Note.to_string root ^ " seventhDiminishedFifth"
    | MajorSixth(root) -> Note.to_string root ^ " majorSixth"
    | MinorSixth(root) -> Note.to_string root ^ " minorSixth"

  let to_intervals chord =
    match chord with
    | MajorTriad(_) -> Relative([Major |. Third; Minor |. Third])
    | MinorTriad(_) -> Relative([Minor |. Third; Major |. Third])
    | AugmentedTriad(_) -> Relative([Major |. Third; Major |. Third])
    | DiminishedTriad(_) -> Relative([Minor |. Third; Minor |. Third])
    | SuspendedTriad(_) -> Relative([Perfect |. Fourth; Major |. Second])
    | PowerChord(_) -> Relative([Perfect |. Fifth])
    | AugmentedPowerChord(_) -> Relative([Augmented |. Fifth])
    | DiminishedPowerChord(_) -> Relative([Diminished |. Fifth])
    | MajorSeventh(_) -> Relative([Major |. Third; Minor |. Third; Major |. Third])
    | DominanteSeventh(_) -> Relative([Major |. Third; Minor |. Third; Minor |. Third])
    | MinorSeventhMajor(_) -> Relative([Minor |. Third; Major |. Third; Major |. Third])
    | MinorSeventh(_) -> Relative([Minor |. Third; Major |. Third; Minor |. Third])
    | AugmentedMajorSeventh(_) -> Relative([Major |. Third; Major |. Third; Minor |. Third])
    | HalfDiminishedSeventh(_) ->
      Absolute([Minor |. Third; Diminished |. Fifth; Minor |. Seventh])
    | DiminishedSeventh(_) ->
      Absolute([Minor |. Third; Diminished |. Fifth; Diminished |. Seventh])
    | SuspendedSeventh(_) -> Absolute([Perfect |. Fourth; Perfect |. Fifth; Minor |. Seventh])
    | SeventhAugmentedFifth(_) ->
      Absolute([Major |. Third; Augmented |. Fifth; Minor |. Seventh])
    | SeventhDiminishedFifth(_) ->
      Absolute([Major |. Third; Diminished |. Fifth; Minor |. Seventh])
    | MajorSixth(_) -> Absolute([Major |.Third; Perfect |. Fifth; Major |. Sixth])
    | MinorSixth(_) -> Absolute([Minor |. Third; Perfect |. Fifth; Major |. Sixth])

  let rec from_intervals root intervals =
    match intervals with
    | Relative(_) ->
      let* absolute_intervals = to_absolute intervals in
      from_intervals root absolute_intervals
    | Absolute(absolute_intervals) ->
      match absolute_intervals with
      | [Third(Minor); Fifth(Perfect)] -> Result.Ok(MinorTriad(root))
      | [Third(Major); Fifth(Perfect)] -> Result.Ok(MajorTriad(root))
      | [Third(Minor); Fifth(Diminished)] -> Result.Ok(DiminishedTriad(root))
      | [Third(Major); Fifth(Augmented)] -> Result.Ok(AugmentedTriad(root))
      | [Third(Minor); Fifth(Perfect); Seventh(Minor)] -> Result.Ok(MinorSeventh(root))
      | [Third(Major); Fifth(Perfect); Seventh(Major)] -> Result.Ok(MajorSeventh(root))
      | [Third(Minor); Fifth(Diminished); Seventh(Minor)] -> Result.Ok(HalfDiminishedSeventh(root))
      | [Third(Major); Fifth(Perfect); Seventh(Minor)] -> Result.Ok(DominanteSeventh(root))
      | [Third(Minor); Fifth(Diminished); Seventh(Diminished)] -> Result.Ok(DiminishedSeventh(root))
      | [Third(Major); Fifth(Augmented); Seventh(Major)] -> Result.Ok(AugmentedMajorSeventh(root))
      | [Third(Minor); Fifth(Perfect); Seventh(Major)] -> Result.Ok(MinorSeventhMajor(root))
      | _  -> Result.Error("Could not find matching chord")

  let to_notes root chord =
    chord
    |. to_intervals
    |> to_notes root
end


module Scale = struct
  open Interval
  open Intervals

  type t =
    | MajorScale of Note.t
    | NaturalMinorScale of Note.t
    | HarmonicMinorScale of Note.t
    | IonianMode of Note.t
    | DorianMode of Note.t
    | PhrygianMode of Note.t
    | LydianMode of Note.t
    | MixolydianMode of Note.t
    | AeolianMode of Note.t
    | IocrianMode of Note.t

  let string_of_scale scale =
    match scale with
    | MajorScale(root) -> Note.to_string root ^ " Major Scale"
    | NaturalMinorScale(root) -> Note.to_string root ^ " Natural Minor"
    | HarmonicMinorScale(root) -> Note.to_string root ^ " Harmonic Minor"
    | IonianMode(root) -> Note.to_string root ^ " Ionial Mode"
    | DorianMode(root) -> Note.to_string root ^ " Dorian Mode"
    | PhrygianMode(root) -> Note.to_string root ^ " Phrygian Mode"
    | LydianMode(root) -> Note.to_string root ^ " Lydian Mode"
    | MixolydianMode(root) -> Note.to_string root ^ " Mixolydian Mode"
    | AeolianMode(root) -> Note.to_string root ^ " Aeolian Mode"
    | IocrianMode(root) -> Note.to_string root ^ " Iocrian Mode"

  let rec get_nth_mode intervals n =
    match n with
    | 0 -> (intervals)
    | _ ->
      match intervals with
      | Relative([])
      | Relative([_]) -> intervals
      | Relative(head :: tail) ->
        get_nth_mode (Relative(List.concat tail [head])) (n - 1)
      | Absolute(_) -> Relative([])  (* should handle absolute case *)

  let rec to_intervals scale =
    match scale with
    | MajorScale(_) ->
      Intervals.Relative([
        Major |. Second;
        Major |. Second;
        Minor |. Second;
        Major |. Second;
        Major |. Second;
        Major |. Second;
        Minor |. Second;
      ])
    | IonianMode(root) -> MajorScale(root) |. to_intervals |. get_nth_mode(0)
    | DorianMode(root) -> MajorScale(root) |. to_intervals |. get_nth_mode(1)
    | PhrygianMode(root) -> MajorScale(root) |. to_intervals |. get_nth_mode(2)
    | LydianMode(root) -> MajorScale(root) |. to_intervals |. get_nth_mode(3)
    | MixolydianMode(root) -> MajorScale(root) |. to_intervals |. get_nth_mode(4)
    | AeolianMode(root) -> MajorScale(root) |. to_intervals |. get_nth_mode(5)
    | IocrianMode(root) -> MajorScale(root) |. to_intervals |. get_nth_mode(6)
    | NaturalMinorScale(_) ->
      Relative([
        Major |. Second;
        Minor |. Second;
        Major |. Second;
        Major |. Second;
        Minor |. Second;
        Major |. Second;
        Major |. Second;
      ])
    | HarmonicMinorScale(_) ->
      Relative([
        Major |. Second;
        Minor |. Second;
        Major |. Second;
        Major |. Second;
        Minor |. Second;
        Augmented |. Second;
        Minor |. Second;
      ])

  let to_notes root scale =
    root |. Intervals.to_notes(scale |> to_intervals)

  let to_string intervals =
    intervals
    |. List.map Interval.to_string
    |> String.concat " -> "
end

module Harmonization = struct
  let rec result_all = function
    | [] -> Result.Ok []
    | Result.Error(e) :: _ -> Result.Error ("Matrix has an Error element: " ^ e)
    | Result.Ok(x) :: xs ->
      let+ rest = result_all xs in
      x :: rest

  (* see https://www.bluesguitarinstitute.com/how-to-harmonize-a-scale/ *)
  let get_harmonization_matrix root scale =
    let intervals = Scale.to_intervals scale in
    let intervals_matrix =
      intervals
      |. Intervals.to_list
      |. List.mapWithIndex(fun i _ -> intervals |. Scale.get_nth_mode(i))
    in
    let* first_row_intervals = intervals_matrix |. List.head |> Stdlib.Option.to_result ~none:"empty matrix" in
    let first_row_notes =  root |. Intervals.to_notes first_row_intervals in
    intervals_matrix
    |. List.mapWithIndex(fun i intervals ->
        let+ row_root = first_row_notes |. List.get(i) |> Stdlib.Option.to_result ~none:"no square matrix" in
        Intervals.to_notes row_root intervals
      )
    |. result_all

  let print_harmonization_matrix matrix =
    matrix
    |. List.map(fun notes -> notes |. Notes.string_of_notes)
    |> String.concat "\n"

  let harmonize_scale root scale =
    let* matrix = get_harmonization_matrix root scale in
    matrix
    |. List.map(fun notes -> notes |. List.keepWithIndex(fun _note index -> List.has [1; 3; 5; 7] (index + 1) (=)))
    |. List.map(fun notes -> (List.headExn notes, Intervals.relative_intervals_of_notes notes))
    |. List.map(fun (root, intervals) -> Chord.from_intervals root intervals)
    |. result_all
end
