module App

open Browser
open Browser.Types
open Elmish
open Thoth.Fetch

open Shared

let [<Literal>] ESC_KEY = 27.
let [<Literal>] ENTER_KEY = 13.
let [<Literal>] ALL_TODOS = "all"
let [<Literal>] ACTIVE_TODOS = "active"
let [<Literal>] COMPLETED_TODOS = "completed"

// The full application state of our todo app.
type Model =
    { entries : Entry []
      editing : int option
      field : string
      uid : int
      visibility : string }

let emptyModel =
    { entries = [||]
      visibility = ALL_TODOS
      editing = None
      field = ""
      uid = 0 }

let newEntry desc id =
  { description = desc
    completed = false
    id = id }



// UPDATE


(** Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
*)
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



// How we update our Model on a given Msg?
let update (msg:Msg) (model:Model) : Model*Cmd<Msg>=
    match msg with
    | Loaded entries ->
        { model with entries = entries }, []

    | Failure err ->
        console.error(err)
        model, []

    | Add ->
        let xs = if System.String.IsNullOrEmpty model.field then
                    model.entries
                 else
                    Array.append model.entries [| newEntry model.field model.uid |]
        { model with
            uid = model.uid + 1
            field = ""
            entries = xs }, []

    | UpdateField str ->
      { model with field = str }, []

    | EditingEntry (id,isEditing) ->
        { model with editing = if isEditing then Some id else None }, []

    | UpdateEntry (id,task) ->
        let updateEntry t =
          if t.id = id then { t with description = task } else t
        { model with entries = Array.map updateEntry model.entries }, []

    | Delete id ->
        { model with entries = Array.filter (fun t -> t.id <> id) model.entries }, []

    | DeleteComplete ->
        { model with entries = Array.filter (fun t -> not t.completed) model.entries }, []

    | Check (id,isCompleted) ->
        let updateEntry t =
          if t.id = id then { t with completed = isCompleted } else t
        { model with entries = Array.map updateEntry model.entries }, []

    | CheckAll isCompleted ->
        let updateEntry t = { t with completed = isCompleted }
        { model with entries = Array.map updateEntry model.entries }, []

    | ChangeVisibility visibility ->
        { model with visibility = visibility }, []

let load () =
    let x () = Thoth.Fetch.Fetch.fetchAs<Entry []> "api/model"
    Cmd.OfPromise.either x () Loaded (string >> Failure)


let init () = emptyModel, load ()

let save (model: Model) : Cmd<Msg> =
    let x (model : Model) =
        promise {
            let! (m: Model) = Thoth.Fetch.Fetch.post ("/api/save", model.entries)
            console.info m
            return ()
        }
    Cmd.OfPromise.attempt x model (string >> Failure)

let updateWithStorage (msg:Msg) (model:Model) =
  match msg with
  // If the Msg is Failure we know the model hasn't changed
  | Failure _ -> model, []
  | _ ->
    let (newModel, cmds) = update msg model
    let save = if newModel.entries <> model.entries then save newModel else Cmd.none
    newModel, Cmd.batch [ save; cmds ]

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
    [ classList [ ("completed", todo.completed); ("editing", editing) ]]
    [ div
        [ ClassName "view" ]
        [ input
            [ ClassName "toggle"
              Type "checkbox"
              Checked todo.completed
              OnChange (fun _ -> Check (todo.id,(not todo.completed)) |> dispatch) ]
          label
            [ OnDoubleClick (fun _ -> EditingEntry (todo.id,true) |> dispatch) ]
            [ str todo.description ]
          button
            [ ClassName "destroy"
              OnClick (fun _-> Delete todo.id |> dispatch) ]
            []
        ]
      input
        [ ClassName "edit"
          DefaultValue todo.description
          Name "title"
          Id ("todo-" + (string todo.id))
          OnInput (fun ev -> UpdateEntry (todo.id, !!ev.target?value) |> dispatch)
          OnBlur (fun _ -> EditingEntry (todo.id,false) |> dispatch)
          onEnter (EditingEntry (todo.id,false)) dispatch ]
    ]

let viewEntries visibility model dispatch =
    let entries = model.entries
    let isVisible todo =
        match visibility with
        | COMPLETED_TODOS -> todo.completed
        | ACTIVE_TODOS -> not todo.completed
        | _ -> true

    let allCompleted =
        Array.forall (fun t -> t.completed) entries

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
           |> Array.map (fun i -> lazyView2 viewEntry (i, model.editing = Some i.id) dispatch)) ]

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
      |> Array.filter (fun t -> t.completed)
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
        [ lazyView2 viewInput model.field dispatch
          lazyView3 viewEntries model.visibility model dispatch
          lazyView3 viewControls model.visibility model.entries dispatch ]
      infoFooter ]

open Elmish.Debug
// App
Program.mkProgram init updateWithStorage view
|> Program.withReactBatched "elmish-app"
#if DEBUG
    |> Program.withDebugger
#endif
|> Program.run