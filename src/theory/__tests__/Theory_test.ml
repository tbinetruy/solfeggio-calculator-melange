open Belt;;
open Jest;;
open Expect;;
open Theory;;

describe "Accidental.to_string" (fun () ->
    let open Accidental in
    test "DoubleFlat" (fun () ->
        expect (DoubleFlat |> to_string) |> toEqual "bb");
    test "Flat" (fun () ->
        expect (Flat |> to_string) |> toEqual "b");
    test "Natural" (fun () ->
        expect (Natural |> to_string) |> toEqual "");
    test "Sharp" (fun () ->
        expect (Sharp |> to_string) |> toEqual "#");
    test "DoubleSharp" (fun () ->
        expect (DoubleSharp |> to_string) |> toEqual "##");
);

describe "Accidental.to_semitones" (fun () ->
    let open Accidental in
    test "DoubleFlat" (fun () ->
        expect (DoubleFlat |> to_semitones) |> toEqual (-2));
    test "Flat" (fun () ->
        expect (Flat |> to_semitones) |> toEqual (-1));
    test "Natural" (fun () ->
        expect (Natural |> to_semitones) |> toEqual 0);
    test "Sharp" (fun () ->
        expect (Sharp |> to_semitones) |> toEqual 1);
    test "DoubleSharp" (fun () ->
        expect (DoubleSharp |> to_semitones) |> toEqual 2);
);

describe "Notes.has_note" (fun () ->
    let open Accidental in
    let open Note in
    let open Notes in
    test "returns true" (fun () ->
        let notes = [C(Natural); D(Natural)] in
        let note = C(Natural) in
        expect (notes |> has_note note) |> toEqual true);
    test "returns false" (fun () ->
        let notes = [C(Natural); D(Natural)] in
        let note = E(Natural) in
        expect (notes |> has_note note) |> toEqual false);
);

describe "Notes.string_of_notes" (fun () ->
    let open Accidental in
    let open Note in
    let open Notes in
    test "Intersect forward" (fun () ->
        let notes = [C(Natural); D(Natural); E(Natural)] in
        expect (notes |> string_of_notes) |> toEqual "C D E");
);

describe "Notes.intersection" (fun () ->
    let open Accidental in
    let open Note in
    let open Notes in
    test "Intersect forward" (fun () ->
        let notes_a = [C(Natural); D(Natural); E(Natural)] in
        let notes_b = [C(Natural); E(Natural)] in
        expect (notes_a |> intersection notes_b) |> toEqual [C(Natural); E(Natural)]);
    test "Intersect backward" (fun () ->
        let notes_a = [C(Natural); E(Natural)] in
        let notes_b = [C(Natural); D(Natural); E(Natural)] in
        expect (notes_a |> intersection notes_b) |> toEqual [C(Natural); E(Natural)]);
    test "Subtracts forward" (fun () ->
        let notes_a = [C(Natural); D(Natural); E(Natural)] in
        let notes_b = [C(Natural)] in
        expect (notes_a |. subtract notes_b) |> toEqual [D(Natural); E(Natural)]);
    test "Subtracts backwards" (fun () ->
        let notes_a = [C(Natural)] in
        let notes_b = [C(Natural); D(Natural); E(Natural)] in
        expect (notes_a |. subtract notes_b) |> toEqual []);
);

describe "Progression" (fun () ->
    let open Accidental in
    let open Note in
    let open Progression in
    test "to_string" (fun () ->
        let notes_a = [C(Natural); E(Natural); G(Natural)] in
        let notes_b = [G(Natural); B(Natural); D(Natural)] in
        let progression = [notes_a; notes_b] in
        expect (progression |> to_string) |> toEqual "C E G | G B D");
);

describe "Interval.PerfectQuality" (fun () ->
    let open Interval.PerfectQuality in
    test "to_semitones" (fun () ->
        let qualifiers = [Diminished; Perfect; Augmented] in
        let expected_semitones = [-1; 0; 1] in
        expect (qualifiers |. List.map to_semitones) |> toEqual expected_semitones);
    test "qualifier_of_semitones success" (fun () ->
        let semitones = [-1; 0; 1] in
        let expected_qualifiers = [Diminished; Perfect; Augmented] |. List.map (fun q -> Result.Ok q) in
        expect (semitones |. List.map quality_of_semitones) |> toEqual expected_qualifiers);
    test "qualifier_of_semitones error" (fun () ->
        let semitones = [-3; -2; 2; 3] in
        let expected_errors = semitones |. List.map (fun _ -> Errors.semitones) in
        expect (semitones |. List.map quality_of_semitones) |> toEqual expected_errors);
    test "to_string" (fun () ->
        let qualifiers = [Diminished; Perfect; Augmented] in
        let expected_strings = ["diminished"; "perfect"; "augmented"] in
        expect (qualifiers |. List.map to_string) |> toEqual expected_strings);
);


describe "Interval.add_intervals" (fun () ->
    let open Interval in
    let open Interval.MajorMinorQuality in
    let open Interval.PerfectQuality in
    test "Perfect Fifth - Major Third" (fun () ->
        let interval_a = Minor |. Third in
        let interval_b = Major |. Third in
        let expected_interval = Perfect |. Fifth in
        expect (add_intervals interval_a interval_b) |> toEqual (Result.Ok(expected_interval)));
);

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
);


describe "Intervals.relative_intervals_of_notes" (fun () ->
    let open Note in
    let open Interval.MajorMinorQuality in
    let open Intervals in
    test "A major" (fun () ->
        let notes = [C(Natural); E(Natural); G(Natural); B(Natural)] in
        let expected_intervals = Relative([Major |. Third; Minor |. Third; Major |. Third]) in
        expect (notes |. relative_intervals_of_notes) |> toEqual expected_intervals);
);

describe "Intervals.absolutely_intervals_of_notes" (fun () ->
    let open Note in
    let open Interval.MajorMinorQuality in
    let open Interval.PerfectQuality in
    let open Intervals in
    test "A major" (fun () ->
        let notes = [C(Natural); E(Natural); G(Natural); B(Natural)] in
        let expected_intervals = Absolute([Major |. Third; Perfect |. Fifth; Major |. Seventh]) in
        expect (notes |. absolute_intervals_of_notes) |> toEqual expected_intervals);
);

describe "Intervals.to_absolute" (fun () ->
    let open Interval.MajorMinorQuality in
    let open Interval.PerfectQuality in
    let open Intervals in
    test "A major" (fun () ->
        let relative_intervals = Relative([Major |. Third; Minor |. Third; Major |. Third]) in
        let absolute_intervals = Absolute([Major |. Third; Perfect |. Fifth; Major |. Seventh]) in
        expect (relative_intervals |. to_absolute) |> toEqual (Result.Ok(absolute_intervals)));
);

describe "Intervals.to_relative" (fun () ->
    let open Interval.MajorMinorQuality in
    let open Interval.PerfectQuality in
    let open Intervals in
    test "A major" (fun () ->
        let absolute_intervals = Absolute([Major |. Third; Perfect |. Fifth; Major |. Seventh]) in
        let relative_intervals = Relative([Major |. Third; Minor |. Third; Major |. Third]) in
        expect (absolute_intervals |. to_relative) |> toEqual (Result.Ok(relative_intervals)));
);

describe "Chord.to_interval" (fun () ->
    let open Note in
    let open Interval.MajorMinorQuality in
    let open Intervals in
    let root = C(Natural) in

    test "A7maj from relative interval" (fun () ->
        let relative_intervals = Relative([Major |. Third; Minor |. Third; Major |. Third]) in
        expect (Chord.from_intervals root relative_intervals) |> toEqual (Result.Ok(Chord.MajorSeventh(root))));

    test "A7maj from absolute interval" (fun () ->
        let absolute_intervals = Absolute([Major |. Third; Perfect |. Fifth ; Major |. Seventh]) in
        expect (Chord.from_intervals root absolute_intervals) |> toEqual (Result.Ok(Chord.MajorSeventh(root))));
);

describe "Scale" (fun () ->
    let open Note in
    let open Chord in
    test "Scale" (fun () ->
        let root = D(Natural) in
        let scale = Scale.MajorScale(root) in

        let _ =
          let+ matrix = root |. Harmonization.get_harmonization_matrix scale in
          Js.Console.log(Harmonization.print_harmonization_matrix matrix);
          matrix
        in

        let chords = Harmonization.harmonize_scale root scale in
        let expected_chords = [
          MajorSeventh(root);
          MinorSeventh(E(Natural));
          MinorSeventh(F(Sharp));
          MajorSeventh(G(Natural));
          DominanteSeventh(A(Natural));
          MinorSeventh(B(Natural));
          HalfDiminishedSeventh(C(Sharp));
        ] in
        let f _ =
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
);
