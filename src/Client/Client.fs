module Client

open Browser.Types
open Elmish
open Elmish.React
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Thoth.Fetch

open Shared

// Entry type comes from Shared module
type Model =
  { Entries : Entry list
    Field : string
    NextId : int }

type Msg =
    | Loaded of Entry list
    | UpdateField of string
    | Add
    | Toggle of Entry
    | Destroy of Entry

let load () =
    promise {
        let! entries =
            Fetch.fetchAs<Entry list> "api/entries"
        return entries
    }

let save (entries: Entry list) =
    promise {
        let! msg =
            Fetch.post("/api/entries", entries)
        return ()
    }

let init () =
    let model : Model =
        { Entries = []
          Field = ""
          NextId = 0 }

    model

let nextId (entries : Entry list) =
    if entries.Length = 0 then 0
    else
        entries
        |> List.map (fun entry -> entry.Id)
        |> List.max
        |> (+) 1

let addEntry model =
    let newEntry =
      { Description = model.Field
        IsCompleted = false
        Id = model.NextId }
    List.append model.Entries [ newEntry ]

let update (msg : Msg) (model : Model) : Model =
    match msg with
    | Loaded entries ->
        { model with
            Entries = entries
            NextId = nextId entries }
    | UpdateField value ->
        { model with Field = value }
    | Add ->
        { NextId = model.NextId + 1
          Field = ""
          Entries = addEntry model }
    | Toggle todo ->
        let toggle t =
            if t.Id <> todo.Id then t
            else { t with IsCompleted = not t.IsCompleted }
        { model with
            Entries = List.map toggle model.Entries }
    | Destroy todo ->
        let predicate t = t.Id <> todo.Id
        { model with
            Entries = List.filter predicate model.Entries }

let updateAndSave (msg:Msg) (model:Model) =
  match msg with
  | _ ->
    let newModel = update msg model
    if newModel.Entries <> model.Entries
    then Promise.start (save newModel.Entries)
    newModel

let [<Literal>] ENTER_KEY = 13.

let internal onEnter msg dispatch =
    function
    | (ev:KeyboardEvent) when ev.keyCode = ENTER_KEY ->
        dispatch msg
    | _ -> ()
    |> OnKeyDown

// VIEW

let viewInput (model:string) dispatch =
    header [ ClassName "header" ] [
        h1 [] [ str "todos" ]
        input [
            ClassName "new-todo"
            Placeholder "What needs to be done?"
            valueOrDefault model
            onEnter Add dispatch
            OnChange (fun (ev:Event) -> !!ev.target?value |> UpdateField |> dispatch)
            AutoFocus true
        ]
    ]

let viewEntry (todo) dispatch =
  li
    [ classList [ ("completed", todo.IsCompleted) ] ]
    [ div
        [ ClassName "view" ]
        [ input
            [ ClassName "toggle"
              Type "checkbox"
              Checked todo.IsCompleted
              OnChange (fun _ -> dispatch (Toggle todo)) ]
          label
            [ ]
            [ str todo.Description ]
          button
            [ ClassName "destroy"
              OnClick (fun _ -> dispatch (Destroy todo)) ]
            [ ] ]
      input
        [ ClassName "edit"
          DefaultValue todo.Description
          Name "title"
          Id ("todo-" + (string todo.Id)) ]
    ]

let viewEntries model dispatch =
    let entries = model.Entries
    let cssVisibility =
        if List.isEmpty entries then "hidden" else "visible"

    section
      [ ClassName "main"
        Style [ Visibility cssVisibility ]]
      [ ul
          [ ClassName "todo-list" ]
          (entries
           |> List.map (fun i -> lazyView2 viewEntry i dispatch)) ]

let view model dispatch =
  div
    [ ClassName "todomvc-wrapper"]
    [ section
        [ ClassName "todoapp" ]
        [ lazyView2 viewInput model.Field dispatch
          lazyView2 viewEntries model dispatch ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkSimple init updateAndSave view
|> Program.withSubscription (fun _ -> Cmd.OfPromise.perform load () Loaded)
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
