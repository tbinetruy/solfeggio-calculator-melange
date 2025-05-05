let ( let* ) promise f =
  Js.Promise.then_ f promise;;


module MyParagraph = struct
  let make ~content =
    p ~children:[React.string content] () [@JSX]
  [@@react.component]
end;;


module Header = struct
  let make ~titles =
    titles
    |. Belt.List.map (fun greeting ->
         h1 ~key:greeting ~children:[React.string greeting] () [@JSX])
    |. Belt.List.toArray
    |. React.array
  [@@react.component]
end;;

let use_data url =
    let (data, set_data) = React.useState (fun _ -> "fetching") in

    let fetch_data url =
      let* response = Fetch.fetch url in
      let* text = Fetch.Response.text response in
      set_data (fun _ -> text);
      Js.Promise.resolve ()
    in

    React.useEffect0 (fun () ->
      let _ = fetch_data url in
      None
    );

    data

module AppOld = struct
  let make () =
    let data = use_data "https://jsonplaceholder.typicode.com/todos/1"  in

    let note = Theory.Note.A(Theory.Accidental.DoubleFlat) |> Theory.Note.to_string in

    let titles = [
      "Hello " ^ World.name ^ "!";
      "This is React!!" ^ note;
    ] in

    div ~children:[
      Header.createElement ~titles:titles () [@JSX];
      MyParagraph.createElement ~content:data () [@JSX];
    ] () [@JSX]
  [@@react.component]
end;;

module StringMap = Stdlib.Map.Make(String)

module Select = struct
  let get_options spec =
    []
    |> StringMap.fold (fun value _ acc ->
        option ~key:value ~children:[React.string(value)] () [@JSX] :: acc
      ) spec
    |> List.rev
    |> Stdlib.Array.of_list
    |> React.array

  let on_change spec event =
    let target = React.Event.Form.target event in
    let _ =
      spec
      |> StringMap.find_opt target##value
      |> Option.map (fun callback -> callback ()) in
    ()

  let make ~spec ~value =
    div ~children:[
      select ~value:value ~onChange:(on_change spec) ~children:[get_options spec] () [@JSX]
    ] () [@JSX]
  [@@react.component]
end;;


module FlexRow = struct
  let flex_row = ReactDOM.Style.make ~display:"flex" ~flexDirection:"row" ()

  let make ~children =
    div ~style:flex_row ~children:[children] () [@JSX]
  [@@react.component]
end;;


module App = struct
  open Theory.Chord
  open Theory.Note
  open Theory.Accidental

  let make () =
    let (root_pitch_class, set_root_pitch_class) = React.useState (fun _ -> C(Natural)) in
    let (accidental, set_accidental) = React.useState (fun _ -> Natural) in
    let (chord, _set_chord_type) = React.useState (fun _ -> None) in

    let set_chord_type f =
      _set_chord_type(f);
      _set_chord_type(f)
    in

    let root_pitch_spec = [
      C(accidental);
      D(accidental);
      E(accidental);
      F(accidental);
      G(accidental);
      A(accidental);
      B(accidental);
    ] |> List.fold_left
        (fun acc el ->
          acc
          |> StringMap.add
            (Theory.Note.to_string el)
            (fun () -> set_root_pitch_class (fun _ -> el))
        )
        StringMap.empty
    in

    let accidental_spec = [
      Flat;
      Natural;
      Sharp;
    ] |> List.fold_left
        (fun acc el ->
           acc
           |> StringMap.add
             (Theory.Accidental.to_string el)
             (fun () -> set_accidental (fun _ -> el))
        )
        StringMap.empty
    in

    let chord_type_spec = [
      MajorTriad(root_pitch_class);
      MinorTriad(root_pitch_class);
    ] |> List.map (fun el -> Some(el))
      |> List.cons None
      |> List.fold_left
        (fun acc el ->
           acc
           |> StringMap.add
             (el |> Option.map Theory.Chord.to_string |> Option.value ~default:"None")
             (fun () -> set_chord_type (fun _ -> el))
        )
        StringMap.empty
    in

    div ~children:[
      h1 ~children:[React.string "Solfeggio calculator"] () [@JSX];
      FlexRow.createElement ~children:[
        React.string "key: ";
        Select.createElement ~spec:root_pitch_spec ~value:(Theory.Note.to_string root_pitch_class) () [@JSX]
      ] () [@JSX];
      FlexRow.createElement ~children:[
        React.string "accidental: ";
        Select.createElement ~spec:accidental_spec ~value:(Theory.Accidental.to_string accidental) () [@JSX]
      ] () [@JSX];
      FlexRow.createElement ~children:[
        React.string "chord: ";
        Select.createElement ~spec:chord_type_spec ~value:(chord |> Option.map Theory.Chord.to_string |> Option.value ~default:"None") () [@JSX]
      ] () [@JSX];
    ] () [@JSX]
  [@@react.component]
end;;

match ReactDOM.querySelector "#root" with
  | Some element ->
      let root = ReactDOM.Client.createRoot element in
      ReactDOM.Client.render root (App.createElement ~children:[] () [@JSX ])
  | None ->
      Js.Console.error
        "Failed to start React: couldn't find the #root element"
