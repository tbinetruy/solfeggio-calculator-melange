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
)
