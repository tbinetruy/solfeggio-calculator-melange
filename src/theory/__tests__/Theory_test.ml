open Belt;;
open Jest;;
open Expect;;
open Theory;;

describe "Accidental.to_string" (fun () ->
    let open Accidental in
    test "Double_flat" (fun () ->
        expect (Double_flat |> to_string) |> toEqual {js|♭♭|js});
    test "Flat" (fun () ->
        expect (Flat |> to_string) |> toEqual {js|♭|js});
    test "Natural" (fun () ->
        expect (Natural |> to_string) |> toEqual "");
    test "Sharp" (fun () ->
        expect (Sharp |> to_string) |> toEqual {js|♯|js});
    test "Double_sharp" (fun () ->
        expect (Double_sharp |> to_string) |> toEqual {js|♯♯|js});
);;

describe "Accidental.to_semitones" (fun () ->
    let open Accidental in
    test "Double_flat" (fun () ->
        expect (Double_flat |> to_semitones) |> toEqual (-2));
    test "Flat" (fun () ->
        expect (Flat |> to_semitones) |> toEqual (-1));
    test "Natural" (fun () ->
        expect (Natural |> to_semitones) |> toEqual 0);
    test "Sharp" (fun () ->
        expect (Sharp |> to_semitones) |> toEqual 1);
    test "Double_sharp" (fun () ->
        expect (Double_sharp |> to_semitones) |> toEqual 2);
);;

describe "Notes.has_note" (fun () ->
    let open Accidental in
    let open Note in
    let open Notes in
    test "returns true" (fun () ->
        let notes = [c Natural; d Natural] in
        let note = c Natural in
        expect (notes |> has_note note) |> toEqual true);
    test "returns false" (fun () ->
        let notes = [c Natural; d Natural] in
        let note = e Natural in
        expect (notes |> has_note note) |> toEqual false);
);;

describe "Notes.string_of_notes" (fun () ->
    let open Accidental in
    let open Note in
    let open Notes in
    test "Intersect forward" (fun () ->
        let notes = [c Natural; d Natural; e Natural] in
        expect (notes |> string_of_notes) |> toEqual "C D E");
);;

describe "Notes.intersection" (fun () ->
    let open Accidental in
    let open Note in
    let open Notes in
    test "Intersect forward" (fun () ->
        let notes_a = [c Natural; d Natural; e Natural] in
        let notes_b = [c Natural; e Natural] in
        expect (notes_a |> intersection notes_b) |> toEqual [c Natural; e Natural]);
    test "Intersect backward" (fun () ->
        let notes_a = [c Natural; e Natural] in
        let notes_b = [c Natural; d Natural; e Natural] in
        expect (notes_a |> intersection notes_b) |> toEqual [c Natural; e Natural]);
    test "Subtracts forward" (fun () ->
        let notes_a = [c Natural; d Natural; e Natural] in
        let notes_b = [c Natural] in
        expect (notes_a |. subtract notes_b) |> toEqual [d Natural; e Natural]);
    test "Subtracts backwards" (fun () ->
        let notes_a = [c Natural] in
        let notes_b = [c Natural; d Natural; e Natural] in
        expect (notes_a |. subtract notes_b) |> toEqual []);
);;

describe "Progression" (fun () ->
    let open Accidental in
    let open Note in
    let open Progression in
    test "to_string" (fun () ->
        let notes_a = [c Natural; e Natural; g Natural] in
        let notes_b = [g Natural; b Natural; d Natural] in
        let progression = [notes_a; notes_b] in
        expect (progression |> to_string) |> toEqual "C E G | G B D");
);;

describe "Interval.PerfectQuality" (fun () ->
    let open Interval.PerfectQuality in
    test "to_semitones" (fun () ->
        let qualifiers = [Diminished; Perfect; Augmented] in
        let expected_semitones = [-1; 0; 1] in
        expect (qualifiers |. List.map to_semitones) |> toEqual expected_semitones);
    test "of_semitones success" (fun () ->
        let semitones = [-1; 0; 1] in
        let expected_qualifiers = [Diminished; Perfect; Augmented] |. List.map (fun q -> Result.Ok q) in
        expect (semitones |. List.map of_semitones) |> toEqual expected_qualifiers);
    test "of_semitones error" (fun () ->
        let semitones = [-3; -2; 2; 3] in
        let expected_errors = semitones |. List.map (fun _ ->
          Result.Error "Semitones greater than 1 or smaller than -1 are not supported.") in
        expect (semitones |. List.map of_semitones) |> toEqual expected_errors);
    test "to_string" (fun () ->
        let qualifiers = [Diminished; Perfect; Augmented] in
        let expected_strings = ["d"; "P"; "A"] in
        expect (qualifiers |. List.map to_string) |> toEqual expected_strings);
);;


describe "Interval.add_intervals" (fun () ->
    let open Interval in
    let open Interval.MajorMinorQuality in
    let open Interval.PerfectQuality in
    test "Perfect Fifth - Major Third" (fun () ->
        let interval_a = Minor |. Third in
        let interval_b = Major |. Third in
        let expected_interval = Perfect |. Fifth in
        expect (add_intervals interval_a interval_b) |> toEqual (Result.Ok(expected_interval)));
);;

describe "Interval.subtract_intervals" (fun () ->
    let open Interval in
    let open Interval.MajorMinorQuality in
    let open Interval.PerfectQuality in
    test "Perfect Fifth - Major Third" (fun () ->
        let interval_a = Perfect |. Fifth in
        let interval_b = Major |. Third in
        let expected_interval = Minor |. Third in
        expect (subtract_intervals interval_a interval_b) |> toEqual (Result.Ok(expected_interval)));

    test "Major Seventh - Perfect Fifth" (fun () ->
        let interval_a = Major |. Seventh in
        let interval_b = Perfect |. Fifth in
        let expected_interval = Major |. Third in
        expect (subtract_intervals interval_a interval_b) |> toEqual (Result.Ok(expected_interval)));
);;


describe "Intervals.relative_intervals_of_notes" (fun () ->
    let open Note in
    let open Interval.MajorMinorQuality in
    let open Intervals in
    test "C major seventh" (fun () ->
        let notes = [c Natural; e Natural; g Natural; b Natural] in
        let expected_intervals = Relative([Major |. Third; Minor |. Third; Major |. Third]) in
        expect (notes |. relative_intervals_of_notes) |> toEqual expected_intervals);
);;

describe "Intervals.absolute_intervals_of_notes" (fun () ->
    let open Note in
    let open Interval.MajorMinorQuality in
    let open Interval.PerfectQuality in
    let open Intervals in
    test "C major seventh" (fun () ->
        let notes = [c Natural; e Natural; g Natural; b Natural] in
        let expected_intervals = Absolute([Major |. Third; Perfect |. Fifth; Major |. Seventh]) in
        expect (notes |. absolute_intervals_of_notes) |> toEqual expected_intervals);
);;

describe "Intervals.to_absolute" (fun () ->
    let open Interval.MajorMinorQuality in
    let open Interval.PerfectQuality in
    let open Intervals in
    test "C major seventh" (fun () ->
        let relative_intervals = Relative([Major |. Third; Minor |. Third; Major |. Third]) in
        let absolute_intervals = Absolute([Major |. Third; Perfect |. Fifth; Major |. Seventh]) in
        expect (relative_intervals |. to_absolute) |> toEqual (Result.Ok(absolute_intervals)));
);;

describe "Intervals.to_relative" (fun () ->
    let open Interval.MajorMinorQuality in
    let open Interval.PerfectQuality in
    let open Intervals in
    test "C major seventh" (fun () ->
        let absolute_intervals = Absolute([Major |. Third; Perfect |. Fifth; Major |. Seventh]) in
        let relative_intervals = Relative([Major |. Third; Minor |. Third; Major |. Third]) in
        expect (absolute_intervals |. to_relative) |> toEqual (Result.Ok(relative_intervals)));
);;

describe "Chord.quality_of_intervals" (fun () ->
    let open Interval.MajorMinorQuality in
    let open Intervals in
    let open Chord in
    let root = Note.c Natural in

    test "Major seventh from relative intervals" (fun () ->
        let relative_intervals = Relative([Major |. Third; Minor |. Third; Major |. Third]) in
        expect (from_intervals root relative_intervals) |> toEqual (Result.Ok(make root Major_seventh)));

    test "Major seventh from absolute intervals" (fun () ->
        let absolute_intervals = Absolute([Major |. Third; Perfect |. Fifth ; Major |. Seventh]) in
        expect (from_intervals root absolute_intervals) |> toEqual (Result.Ok(make root Major_seventh)));
);;

describe "Scale harmonization" (fun () ->
    let open Note in
    let open Chord in
    test "D Major scale" (fun () ->
        let root = d Natural in

        let _ =
          let open Result_syntax in
          let+ matrix = Harmonization.get_harmonization_matrix root Scale.Major in
          Js.Console.log(Harmonization.print_harmonization_matrix matrix);
          matrix
        in

        let chords = Harmonization.harmonize_scale root Scale.Major in
        let expected_chords = [
          make root Major_seventh;
          make (e Natural) Minor_seventh;
          make (f Sharp) Minor_seventh;
          make (g Natural) Major_seventh;
          make (a Natural) Dominant_seventh;
          make (b Natural) Minor_seventh;
          make (c Sharp) Half_diminished_seventh;
        ] in
        let f _ =
          let open Result_syntax in
          let+ chords = chords in
          let str = chords
          |. List.map (fun chord -> chord |. to_string)
          |> String.concat "\n"
          in
          Js.Console.log(str);
          str
        in
        let _ = f 1 in
        expect (chords) |> toEqual (Result.Ok(expected_chords)));
);;


describe "Compound intervals" (fun () ->
    let open Interval in
    let mmq = Interval.MajorMinorQuality.Major in
    let mmq_minor = Interval.MajorMinorQuality.Minor in

    test "Ninth to_semitones" (fun () ->
        expect (mmq |. Ninth |> to_semitones) |> toEqual 14);
    test "Minor ninth to_semitones" (fun () ->
        expect (mmq_minor |. Ninth |> to_semitones) |> toEqual 13);
    test "Perfect eleventh to_semitones" (fun () ->
        expect (PerfectQuality.Perfect |. Eleventh |> to_semitones) |> toEqual 17);
    test "Major thirteenth to_semitones" (fun () ->
        expect (mmq |. Thirteenth |> to_semitones) |> toEqual 21);

    test "from_semitones Ninth" (fun () ->
        expect (from_semitones 9 14) |> toEqual (Result.Ok (mmq |. Ninth)));
    test "from_semitones minor Ninth" (fun () ->
        expect (from_semitones 9 13) |> toEqual (Result.Ok (mmq_minor |. Ninth)));
    test "from_semitones Eleventh" (fun () ->
        expect (from_semitones 11 17) |> toEqual (Result.Ok (PerfectQuality.Perfect |. Eleventh)));
    test "from_semitones Thirteenth" (fun () ->
        expect (from_semitones 13 21) |> toEqual (Result.Ok (mmq |. Thirteenth)));

    test "to_string Ninth" (fun () ->
        expect (mmq |. Ninth |> to_string) |> toEqual "M9");
    test "to_string Eleventh" (fun () ->
        expect (PerfectQuality.Perfect |. Eleventh |> to_string) |> toEqual "P11");
    test "to_string Thirteenth" (fun () ->
        expect (mmq |. Thirteenth |> to_string) |> toEqual "M13");

    test "next_note Ninth from C" (fun () ->
        let note = next_note (Note.c Natural) (mmq |. Ninth) in
        expect (Note.to_string note) |> toEqual "D");
    test "next_note minor Ninth from C" (fun () ->
        let note = next_note (Note.c Natural) (mmq_minor |. Ninth) in
        expect (Note.to_string note) |> toEqual {js|D♭|js});
    test "next_note Thirteenth from C" (fun () ->
        let note = next_note (Note.c Natural) (mmq |. Thirteenth) in
        expect (Note.to_string note) |> toEqual "A");
);;


describe "Extended chord notes" (fun () ->
    let open Note in
    let open Chord in
    let root = c Natural in

    test "Dominant ninth" (fun () ->
        let notes = to_notes root Dominant_ninth in
        expect (Notes.string_of_notes notes) |> toEqual {js|C E G B♭ D|js});
    test "Minor ninth" (fun () ->
        let notes = to_notes root Minor_ninth in
        expect (Notes.string_of_notes notes) |> toEqual {js|C E♭ G B♭ D|js});
    test "Dominant eleventh" (fun () ->
        let notes = to_notes root Dominant_eleventh in
        expect (Notes.string_of_notes notes) |> toEqual {js|C E G B♭ D F|js});
    test "Dominant thirteenth" (fun () ->
        let notes = to_notes root Dominant_thirteenth in
        expect (Notes.string_of_notes notes) |> toEqual {js|C E G B♭ D A|js});
    test "Major six nine" (fun () ->
        let notes = to_notes root Major_six_nine in
        expect (Notes.string_of_notes notes) |> toEqual "C E G A D");
    test "Minor six nine" (fun () ->
        let notes = to_notes root Minor_six_nine in
        expect (Notes.string_of_notes notes) |> toEqual {js|C E♭ G A D|js});
);;


describe "Extended chord recognition" (fun () ->
    let open Interval in
    let mm = Interval.MajorMinorQuality.Major in
    let mmm = Interval.MajorMinorQuality.Minor in
    let pp = Interval.PerfectQuality.Perfect in
    let open Chord in

    test "Dominant ninth from absolute intervals" (fun () ->
        let intervals = Intervals.Absolute [
          mm |. Third; pp |. Fifth; mmm |. Seventh; mm |. Ninth
        ] in
        expect (quality_of_intervals intervals) |> toEqual (Result.Ok Dominant_ninth));
    test "Minor eleventh from absolute intervals" (fun () ->
        let intervals = Intervals.Absolute [
          mmm |. Third; pp |. Fifth; mmm |. Seventh; mm |. Ninth; pp |. Eleventh
        ] in
        expect (quality_of_intervals intervals) |> toEqual (Result.Ok Minor_eleventh));
    test "Dominant thirteenth from absolute intervals" (fun () ->
        let intervals = Intervals.Absolute [
          mm |. Third; pp |. Fifth; mmm |. Seventh; mm |. Ninth; mm |. Thirteenth
        ] in
        expect (quality_of_intervals intervals) |> toEqual (Result.Ok Dominant_thirteenth));
    test "Major six nine from absolute intervals" (fun () ->
        let intervals = Intervals.Absolute [
          mm |. Third; pp |. Fifth; mm |. Sixth; mm |. Ninth
        ] in
        expect (quality_of_intervals intervals) |> toEqual (Result.Ok Major_six_nine));
);;


describe "Absolute intervals with compound promotion" (fun () ->
    let open Note in
    let open Interval in
    let mm = Interval.MajorMinorQuality.Major in
    let mmm = Interval.MajorMinorQuality.Minor in
    let pp = Interval.PerfectQuality.Perfect in

    test "C dom9 shows M9 not M2" (fun () ->
        let notes = Chord.to_notes (c Natural) Chord.Dominant_ninth in
        let abs = Intervals.absolute_intervals_of_notes notes in
        let expected = Intervals.Absolute [
          mm |. Third; pp |. Fifth; mmm |. Seventh; mm |. Ninth
        ] in
        expect abs |> toEqual expected);
    test "C dom13 shows M13 not M6" (fun () ->
        let notes = Chord.to_notes (c Natural) Chord.Dominant_thirteenth in
        let abs = Intervals.absolute_intervals_of_notes notes in
        let expected = Intervals.Absolute [
          mm |. Third; pp |. Fifth; mmm |. Seventh; mm |. Ninth; mm |. Thirteenth
        ] in
        expect abs |> toEqual expected);
);;


describe "Note.assign_octaves" (fun () ->
    let open Note in

    test "D major scale: C# is octave 5" (fun () ->
        let notes = Scale.to_notes (d Natural) Scale.Major in
        let noted = assign_octaves notes in
        let last_two = noted |. List.reverse |. List.take 2 |. Belt.Option.getExn in
        expect last_two |> toEqual [(d Natural, 5); (c Sharp, 5)]);

    test "C major scale: octave C is octave 5" (fun () ->
        let notes = Scale.to_notes (c Natural) Scale.Major in
        let noted = assign_octaves notes in
        let last = noted |. List.reverse |. List.head |. Belt.Option.getExn in
        expect last |> toEqual (c Natural, 5));

    test "C dom13: 9th and 13th are octave 5" (fun () ->
        let notes = Chord.to_notes (c Natural) Chord.Dominant_thirteenth in
        let noted = assign_octaves notes in
        let octaves = noted |. List.map snd in
        expect octaves |> toEqual [4; 4; 4; 4; 5; 5]);

    test "C dom13 without 9th: 13th stays octave 5" (fun () ->
        let notes = Chord.to_notes (c Natural) Chord.Dominant_thirteenth in
        (* Remove 9th (index 4, D) *)
        let filtered = notes |. List.toArray
          |. Array.keepWithIndex (fun _n i -> i <> 4)
          |. List.fromArray in
        let noted = assign_octaves filtered in
        let octaves = noted |. List.map snd in
        expect octaves |> toEqual [4; 4; 4; 4; 5]);

    test "Cmaj7: all octave 4" (fun () ->
        let notes = Chord.to_notes (c Natural) Chord.Major_seventh in
        let noted = assign_octaves notes in
        let octaves = noted |. List.map snd in
        expect octaves |> toEqual [4; 4; 4; 4]);
);;


describe "Interval.to_chord_label" (fun () ->
    let open Interval in
    let mm = Interval.MajorMinorQuality.Major in
    let mmm = Interval.MajorMinorQuality.Minor in
    let pp = Interval.PerfectQuality.Perfect in
    let pd = Interval.PerfectQuality.Diminished in
    let pa = Interval.PerfectQuality.Augmented in

    test "Major third" (fun () ->
        expect (mm |. Third |> to_chord_label) |> toEqual "3");
    test "Perfect fifth" (fun () ->
        expect (pp |. Fifth |> to_chord_label) |> toEqual "5");
    test "Major ninth" (fun () ->
        expect (mm |. Ninth |> to_chord_label) |> toEqual "9");
    test "Minor third" (fun () ->
        expect (mmm |. Third |> to_chord_label) |> toEqual {js|♭3|js});
    test "Minor seventh" (fun () ->
        expect (mmm |. Seventh |> to_chord_label) |> toEqual {js|♭7|js});
    test "Diminished fifth" (fun () ->
        expect (pd |. Fifth |> to_chord_label) |> toEqual {js|♭5|js});
    test "Augmented fifth" (fun () ->
        expect (pa |. Fifth |> to_chord_label) |> toEqual {js|♯5|js});
    test "Unison is R" (fun () ->
        expect (Unison |> to_chord_label) |> toEqual "R");
    test "Octave is R" (fun () ->
        expect (Octave |> to_chord_label) |> toEqual "R");
);;


describe "Chord.tone_labels" (fun () ->
    let open Chord in

    test "Dominant ninth" (fun () ->
        expect (tone_labels Dominant_ninth) |> toEqual [{js|R|js}; "3"; "5"; {js|♭7|js}; "9"]);
    test "Dominant thirteenth" (fun () ->
        expect (tone_labels Dominant_thirteenth) |> toEqual ["R"; "3"; "5"; {js|♭7|js}; "9"; "13"]);
    test "Major seventh" (fun () ->
        expect (tone_labels Major_seventh) |> toEqual ["R"; "3"; "5"; "7"]);
);;


describe "Chord.has_minor_third" (fun () ->
    let open Chord in

    test "Minor_triad" (fun () ->
        expect (has_minor_third Minor_triad) |> toEqual true);
    test "Minor_seventh" (fun () ->
        expect (has_minor_third Minor_seventh) |> toEqual true);
    test "Half_diminished_seventh" (fun () ->
        expect (has_minor_third Half_diminished_seventh) |> toEqual true);
    test "Diminished_seventh" (fun () ->
        expect (has_minor_third Diminished_seventh) |> toEqual true);
    test "Minor_ninth" (fun () ->
        expect (has_minor_third Minor_ninth) |> toEqual true);
    test "Major_triad" (fun () ->
        expect (has_minor_third Major_triad) |> toEqual false);
    test "Dominant_seventh" (fun () ->
        expect (has_minor_third Dominant_seventh) |> toEqual false);
    test "Suspended_triad" (fun () ->
        expect (has_minor_third Suspended_triad) |> toEqual false);
);;


describe "VexFlow conversions" (fun () ->
    let open Note in

    test "to_vexflow_key C" (fun () ->
        expect (to_vexflow_key (c Natural)) |> toEqual "C");
    test "to_vexflow_key F#" (fun () ->
        expect (to_vexflow_key (f Sharp)) |> toEqual "F#");
    test "to_vexflow_key Bb" (fun () ->
        expect (to_vexflow_key (b Flat)) |> toEqual "Bb");
    test "to_vexflow_note c/4" (fun () ->
        expect (to_vexflow_note (c Natural) 4) |> toEqual "c/4");
    test "to_vexflow_note f#/4" (fun () ->
        expect (to_vexflow_note (f Sharp) 4) |> toEqual "f#/4");
    test "to_tonejs_note C4" (fun () ->
        expect (to_tonejs_note (c Natural) 4) |> toEqual "C4");
    test "to_tonejs_note F#4" (fun () ->
        expect (to_tonejs_note (f Sharp) 4) |> toEqual "F#4");
    test "Accidental.to_vexflow sharp" (fun () ->
        expect (Accidental.to_vexflow Sharp) |> toEqual "#");
    test "Accidental.to_vexflow flat" (fun () ->
        expect (Accidental.to_vexflow Flat) |> toEqual "b");
);;


describe "Scale.to_vexflow_key" (fun () ->
    let open Note in

    test "C major" (fun () ->
        expect (Scale.to_vexflow_key (c Natural) Scale.Major) |> toEqual "C");
    test "D major" (fun () ->
        expect (Scale.to_vexflow_key (d Natural) Scale.Major) |> toEqual "D");
    test "F# major" (fun () ->
        expect (Scale.to_vexflow_key (f Sharp) Scale.Major) |> toEqual "F#");
    test "A natural minor" (fun () ->
        expect (Scale.to_vexflow_key (a Natural) Scale.Natural_minor) |> toEqual "Am");
    test "C harmonic minor" (fun () ->
        expect (Scale.to_vexflow_key (c Natural) Scale.Harmonic_minor) |> toEqual "Cm");
    test "D dorian derives C major" (fun () ->
        expect (Scale.to_vexflow_key (d Natural) Scale.Dorian) |> toEqual "C");
    test "E phrygian derives C major" (fun () ->
        expect (Scale.to_vexflow_key (e Natural) Scale.Phrygian) |> toEqual "C");
    test "G mixolydian derives C major" (fun () ->
        expect (Scale.to_vexflow_key (g Natural) Scale.Mixolydian) |> toEqual "C");
)
