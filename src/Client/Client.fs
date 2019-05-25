module Client

open Browser
open Browser.Types
open Elmish
open Elmish.React
open Fable.Core
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Thoth.Json

open Shared

let [<Literal>] ENTER_KEY = 13.

// Entry type comes from Shared module
type Model =
  { Entries : Entry []
    Field : string
    NextId : int }

type Msg =
    | Loaded of Entry []
    | Failure of string
    | UpdateField of string
    | Add

let emptyModel =
  { Entries = [||]
    Field = ""
    NextId = 0 }

let newEntry desc id =
  { Description = desc
    IsCompleted = false
    Id = id }

let load () =
    promise {
        let! entries =
            Fetch.fetchAs<Entry []> "api/entries"
        return entries
    }

let save (entries: Entry []) =
    promise {
        let! msg =
            Fetch.post("/api/entries", entries)
        return ()
    }

let init () =
    emptyModel, Cmd.OfPromise.either load () Loaded (string >> Failure)

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | Loaded entries ->
        { model with
            Entries = entries
            NextId =
                if entries.Length = 0 then 0
                else
                    entries |> Array.map (fun e -> e.Id) |> Array.max |> (+) 1 }, Cmd.none
    | Failure e ->
        console.error e
        model, Cmd.none
    | UpdateField str ->
      { model with Field = str }, Cmd.none
    | Add ->
        let xs = if System.String.IsNullOrEmpty model.Field then
                    model.Entries
                 else
                    Array.append model.Entries [| newEntry model.Field model.NextId |]
        { model with
            NextId = model.NextId + 1
            Field = ""
            Entries = xs }, []

let updateWithSave (msg:Msg) (model:Model) =
  match msg with
  | _ ->
    let (newModel, cmds) = update msg model
    let cmd =
        if newModel.Entries <> model.Entries
        then Cmd.OfPromise.attempt save newModel.Entries (string >> Failure)
        else Cmd.none
    newModel, Cmd.batch [ cmd; cmds ]

open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Elmish.React

let internal onEnter msg dispatch =
    function
    | (ev:KeyboardEvent) when ev.keyCode = ENTER_KEY ->
        ev.target?value <- ""
        dispatch msg
    | _ -> ()
    |> OnKeyDown

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

let internal classList classes =
    classes
    |> List.fold (fun complete -> function | (name,true) -> complete + " " + name | _ -> complete) ""
    |> ClassName

let viewEntry (todo) dispatch =
  li
    [ classList [ ("completed", todo.IsCompleted) ] ]
    [ div
        [ ClassName "view" ]
        [ label
            [ ]
            [ str todo.Description ] ]
      input
        [ ClassName "edit"
          DefaultValue todo.Description
          Name "title"
          Id ("todo-" + (string todo.Id)) ]
    ]

let viewEntries model dispatch =
    let entries = model.Entries
    let cssVisibility =
        if Array.isEmpty entries then "hidden" else "visible"

    section
      [ ClassName "main"
        Style [ Visibility cssVisibility ]]
      [ ul
          [ ClassName "todo-list" ]
          (entries
           |> Array.map (fun i -> lazyView2 viewEntry i dispatch)) ]

let viewControlsCount entriesLeft =
  let item =
      if entriesLeft = 1 then " item" else " items"

  span
      [ ClassName "todo-count" ]
      [ strong [] [ str (string entriesLeft) ]
        str (item + " left") ]

let viewControls visibility entries dispatch =
  let entriesCompleted =
      entries
      |> Array.filter (fun t -> t.IsCompleted)
      |> Array.length

  let entriesLeft =
      Array.length entries - entriesCompleted

  footer
      [ ClassName "footer"
        Hidden (Array.isEmpty entries) ]
      [ lazyView viewControlsCount entriesLeft ]

let safeComponents =
    let components =
        span [ ]
           [
             a [ Href "https://saturnframework.github.io" ] [ str "Saturn" ]
             str ", "
             a [ Href "http://fable.io" ] [ str "Fable" ]
             str ", "
             a [ Href "https://elmish.github.io/elmish/" ] [ str "Elmish" ]
           ]

    span [ ]
        [ strong [] [ str "SAFE Todo MVC" ]
          str " powered by: "
          components ]

let infoFooter =
  footer [ ClassName "info" ]
    [ safeComponents ]

let view model dispatch =
  div
    [ ClassName "todomvc-wrapper"]
    [ section
        [ ClassName "todoapp" ]
        [ lazyView2 viewInput model.Field dispatch
          lazyView2 viewEntries model dispatch ]
      infoFooter ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init updateWithSave view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
