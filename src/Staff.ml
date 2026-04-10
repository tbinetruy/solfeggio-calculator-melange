open Theory

let render_chord container notes key_signature =
  let open Vexflow_bindgen in
  let renderer = Renderer.make container Renderer.svg_backend in
  Renderer.resize renderer 300 150 |> ignore;
  let context = Renderer.get_context renderer in
  let stave = Stave.make 0 0 280 in
  let _ =
    stave
    |> fun s -> Stave.add_clef s "treble"
    |> fun s -> Stave.add_key_signature s key_signature
    |> fun s -> Stave.set_context s context
  in
  Stave.draw stave;
  let noted = Note.assign_octaves notes in
  let keys = noted
    |> Stdlib.List.map (fun (note, octave) -> Note.to_vexflow_note note octave)
    |> Stdlib.Array.of_list
  in
  let chord = StaveNote.make (StaveNote.config ~keys ~duration:"w") in
  (* Add accidentals for notes that need them *)
  let _ = noted |> Stdlib.List.mapi (fun i (note, _octave) ->
    let acc_str = Theory.Accidental.to_vexflow note.Note.accidental in
    if acc_str <> "" then
      StaveNote.add_modifier chord (Accidental.make acc_str) i |> ignore
  ) in
  let voice = Voice.make (Voice.config ~num_beats:4 ~beat_value:4) in
  let _ = Voice.add_tickables voice [|chord|] in
  let formatter = Formatter.make () in
  let _ = Formatter.join_voices formatter [|voice|] in
  let _ = Formatter.format formatter [|voice|] 250 in
  Voice.draw voice context stave

let make ~notes ~key_signature =
  let container_ref = React.useRef Js.Nullable.null in
  React.useEffect2 (fun () ->
    (match Js.Nullable.toOption container_ref.current with
     | Some el ->
       (* Clear previous render *)
       let _ = [%mel.raw {|el.innerHTML = ""|}] in
       (match notes with
        | _ :: _ -> render_chord el notes key_signature
        | [] -> ())
     | None -> ());
    None
  ) (notes, key_signature);
  div ~ref:(ReactDOM.Ref.domRef container_ref)
    ~style:(ReactDOM.Style.make ~minHeight:"150px" ())
    ~children:[] () [@JSX]
[@@react.component]
