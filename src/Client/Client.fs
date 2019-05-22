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

let [<Literal>] ESC_KEY = 27.
let [<Literal>] ENTER_KEY = 13.
let [<Literal>] ALL_TODOS = "all"
let [<Literal>] ACTIVE_TODOS = "active"
let [<Literal>] COMPLETED_TODOS = "completed"

type Model =
  { Entries : Entry []
    Editing : int option
    Field : string
    NextId : int
    Visibility : string }

type Msg =
    | Loaded of Entry []
    | Failure of string
    | UpdateField of string
    | EditingEntry of int*bool
    | UpdateEntry of int*string
    | Add
    | Delete of int
    | DeleteComplete
    | Check of int*bool
    | CheckAll of bool
    | ChangeVisibility of string


let emptyModel =
  { Entries = [||]
    Visibility = ALL_TODOS
    Editing = None
    Field = ""
    NextId = 0 }

let newEntry desc id =
  { Description = desc
    IsCompleted = false
    Id = id }

let load () : JS.Promise<Entry []>  =
    promise {
        let! entries = Fetch.fetchAs<Entry []> "api/entries"
        return entries
    }

let save (entries : Entry []) : JS.Promise<unit> =
    promise {
        let! (msg : string) = Fetch.post ("/api/entries", entries)
        return ()
    }

let init () =
    emptyModel, Cmd.OfPromise.either load () Loaded (string >> Failure)

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | Failure msg ->
        console.error msg
        model, Cmd.none
    | EditingEntry (id, editing) ->
        { model with Editing = if editing then Some id else None }, Cmd.none
    | UpdateEntry (id, value) ->
        let update entry =
            if entry.Id = id then { entry with Description = value } else entry
        { model with Entries = Array.map update model.Entries }, Cmd.none
    | DeleteComplete ->
        { model with Entries = model.Entries |> Array.filter (fun e -> not e.IsCompleted) }, Cmd.none
    | Loaded entries ->
        { model with
            Entries = entries
            NextId = entries |> Array.map (fun e -> e.Id) |> Array.max |> (+) 1 }, Cmd.none
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
    | CheckAll isCompleted ->
        let updateEntry t = { t with IsCompleted = isCompleted }
        { model with Entries = Array.map updateEntry model.Entries }, []

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

let viewEntry (todo, editing) dispatch =
  li
    [ classList [ ("completed", todo.IsCompleted); ("editing", editing) ]]
    [ div
        [ ClassName "view" ]
        [ input
            [ ClassName "toggle"
              Type "checkbox"
              Checked todo.IsCompleted
              OnChange (fun _ -> Check (todo.Id,(not todo.IsCompleted)) |> dispatch) ]
          label
            [ OnDoubleClick (fun _ -> EditingEntry (todo.Id,true) |> dispatch) ]
            [ str todo.Description ]
          button
            [ ClassName "destroy"
              OnClick (fun _-> Delete todo.Id |> dispatch) ]
            []
        ]
      input
        [ ClassName "edit"
          DefaultValue todo.Description
          Name "title"
          Id ("todo-" + (string todo.Id))
          OnInput (fun ev -> UpdateEntry (todo.Id, !!ev.target?value) |> dispatch)
          OnBlur (fun _ -> EditingEntry (todo.Id,false) |> dispatch)
          onEnter (EditingEntry (todo.Id,false)) dispatch ]
    ]

let viewEntries visibility model dispatch =
    let entries = model.Entries
    let isVisible todo =
        match visibility with
        | COMPLETED_TODOS -> todo.IsCompleted
        | ACTIVE_TODOS -> not todo.IsCompleted
        | _ -> true

    let allCompleted =
        Array.forall (fun t -> t.IsCompleted) entries

    let cssVisibility =
        if Array.isEmpty entries then "hidden" else "visible"

    section
      [ ClassName "main"
        Style [ Visibility cssVisibility ]]
      [ input
          [ ClassName "toggle-all"
            Type "checkbox"
            Name "toggle"
            Checked allCompleted
            OnChange (fun _ -> CheckAll (not allCompleted) |> dispatch)]
        label
          [ HtmlFor "toggle-all" ]
          [ str "Mark all as complete" ]
        ul
          [ ClassName "todo-list" ]
          (entries
           |> Array.filter isVisible
           |> Array.map (fun i -> lazyView2 viewEntry (i, model.Editing = Some i.Id) dispatch)) ]

// VIEW CONTROLS AND FOOTER
let visibilitySwap uri visibility actualVisibility dispatch =
  li
    [ OnClick (fun _ -> ChangeVisibility visibility |> dispatch) ]
    [ a [ Href uri
          classList ["selected", visibility = actualVisibility] ]
          [ str visibility ] ]

let viewControlsFilters visibility dispatch =
  ul
    [ ClassName "filters" ]
    [ visibilitySwap "#/" ALL_TODOS visibility dispatch
      str " "
      visibilitySwap "#/active" ACTIVE_TODOS visibility dispatch
      str " "
      visibilitySwap "#/completed" COMPLETED_TODOS visibility dispatch ]

let viewControlsCount entriesLeft =
  let item =
      if entriesLeft = 1 then " item" else " items"

  span
      [ ClassName "todo-count" ]
      [ strong [] [ str (string entriesLeft) ]
        str (item + " left") ]

let viewControlsClear entriesCompleted dispatch =
  button
    [ ClassName "clear-completed"
      Hidden (entriesCompleted = 0)
      OnClick (fun _ -> DeleteComplete |> dispatch)]
    [ str ("Clear completed (" + (string entriesCompleted) + ")") ]

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
      [ lazyView viewControlsCount entriesLeft
        lazyView2 viewControlsFilters visibility dispatch
        lazyView2 viewControlsClear entriesCompleted dispatch ]

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
    [ p []
        [ str "Double-click to edit a todo" ]
      safeComponents
    ]

let view model dispatch =
  div
    [ ClassName "todomvc-wrapper"]
    [ section
        [ ClassName "todoapp" ]
        [ lazyView2 viewInput model.Field dispatch
          lazyView3 viewEntries model.Visibility model dispatch
          lazyView3 viewControls model.Visibility model.Entries dispatch ]
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
