open Theory

let make_stave_note noted duration =
  let open Vexflow_bindgen in
  let keys = noted
    |> Stdlib.List.map (fun (note, octave) -> Note.to_vexflow_note note octave)
    |> Stdlib.Array.of_list
  in
  StaveNote.make (StaveNote.config ~keys ~duration)

let render_staff container ~width ~key_signature ~stave_notes ~num_beats =
  let open Vexflow_bindgen in
  let renderer = Renderer.make container Renderer.svg_backend in
  Renderer.resize renderer width 150 |> ignore;
  let context = Renderer.get_context renderer in
  let stave = Stave.make 0 0 (width - 20) in
  let _ =
    stave
    |> fun s -> Stave.add_clef s "treble"
    |> fun s -> Stave.add_key_signature s key_signature
    |> fun s -> Stave.set_context s context
  in
  Stave.draw stave;
  let voice = Voice.make (Voice.config ~num_beats ~beat_value:4) in
  let _ = Voice.set_mode voice Voice.soft_mode in
  let _ = Voice.add_tickables voice stave_notes in
  apply_accidentals [|voice|] key_signature;
  let formatter = Formatter.make () in
  let _ = Formatter.join_voices formatter [|voice|] in
  let _ = Formatter.format formatter [|voice|] (width - 120) in
  Voice.draw voice context stave

let render_notation container notes key_signature =
  let noted = Note.assign_octaves notes in
  let n = Stdlib.List.length noted in
  if n > 4 then
    let stave_notes = noted
      |> Stdlib.List.map (fun pair -> make_stave_note [pair] "q")
      |> Stdlib.Array.of_list
    in
    render_staff container
      ~width:(120 + n * 55) ~key_signature ~stave_notes ~num_beats:n
  else
    let stave_notes = [|make_stave_note noted "w"|] in
    render_staff container
      ~width:300 ~key_signature ~stave_notes ~num_beats:4

let play_notes notes =
  let open Tone_bindgen in
  let noted = Note.assign_octaves notes in
  let n = Stdlib.List.length noted in
  let synth = PolySynth.make () |> PolySynth.to_destination in
  let _ = start () |> Js.Promise.then_ (fun () ->
    if n > 4 then begin
      let t = now () in
      noted |> Stdlib.List.iteri (fun i (note, octave) ->
        PolySynth.trigger_attack_release_at synth
          [|Note.to_tonejs_note note octave|] "8n" (t +. Float.of_int i *. 0.3)
      )
    end else begin
      let tone_notes = noted
        |> Stdlib.List.map (fun (note, octave) -> Note.to_tonejs_note note octave)
        |> Stdlib.Array.of_list
      in
      PolySynth.trigger_attack_release synth tone_notes "2n"
    end;
    Js.Promise.resolve ()
  ) in
  ()

let play_button_style = ReactDOM.Style.make
  ~background:"none" ~border:"1px solid #ccc" ~borderRadius:"50%"
  ~width:"2rem" ~height:"2rem" ~cursor:"pointer"
  ~display:"flex" ~alignItems:"center" ~justifyContent:"center"
  ~fontSize:"0.9rem" ~color:"#555" ~flexShrink:"0" ()

let wrapper_style = ReactDOM.Style.make
  ~display:"flex" ~alignItems:"center" ~gap:"0.5rem" ()

let make ~notes ~key_signature =
  let container_ref = React.useRef Js.Nullable.null in
  React.useEffect2 (fun () ->
    (match Js.Nullable.toOption container_ref.current with
     | Some el ->
       let _ = [%mel.raw {|el.innerHTML = ""|}] in
       (match notes with
        | _ :: _ -> render_notation el notes key_signature
        | [] -> ())
     | None -> ());
    None
  ) (notes, key_signature);
  div ~style:wrapper_style ~children:[
    div ~ref:(ReactDOM.Ref.domRef container_ref)
      ~style:(ReactDOM.Style.make ~minHeight:"150px" ~flexGrow:"1" ())
      ~children:[] () [@JSX];
    button ~style:play_button_style
      ~onClick:(fun _ -> play_notes notes)
      ~children:[React.string {js|▶|js}] () [@JSX];
  ] () [@JSX]
[@@react.component]
