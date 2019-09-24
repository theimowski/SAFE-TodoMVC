module Client

open System
open Browser.Types
open Elmish
open Elmish.React
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Thoth.Fetch

open Shared

// Model

type Model =
    { Todos : Todo list
      Input : string }

// Initial model

let init todos : Model =
    { Todos = todos
      Input = "" }

// Messages

type Msg =
    | UpdateInput of string
    | Add
    | Toggle of Guid
    | Destroy of Guid

// Update model

let update (msg : Msg) (model : Model) : Model =
    match msg with
    | UpdateInput value ->
        { model with Input = value }
    | Add ->
        let newTodo =
          { Description = model.Input
            IsCompleted = false
            Id = Guid.NewGuid() }
        { Input = ""
          Todos = List.append model.Todos [ newTodo ] }
    | Toggle id ->
        let toggle todo =
            if todo.Id <> id then todo
            else
              { todo with IsCompleted = not todo.IsCompleted }
        { model with Todos = List.map toggle model.Todos }
    | Destroy id ->
        let filter todo = todo.Id <> id
        { model with Todos = List.filter filter model.Todos }

// Helpers

let [<Literal>] ENTER_KEY = 13.

let internal onEnter msg dispatch =
    function
    | (ev:KeyboardEvent) when ev.keyCode = ENTER_KEY ->
        dispatch msg
    | _ -> ()
    |> OnKeyDown

// View (React)

let viewInput (model: Model) dispatch =
    header [ ClassName "header" ] [
        h1 [] [ str "todos" ]
        input [
            ClassName "new-todo"
            Placeholder "What needs to be done?"
            valueOrDefault model.Input
            onEnter Add dispatch
            OnChange (fun (ev:Event) -> !!ev.target?value |> UpdateInput |> dispatch)
            AutoFocus true
        ]
    ]

let viewTodo (todo) dispatch =
  li
    [ classList [ ("completed", todo.IsCompleted) ] ]
    [ div
        [ ClassName "view" ]
        [ input
            [ Type "checkbox"
              ClassName "toggle"
              Checked todo.IsCompleted
              OnChange (fun _ -> Toggle todo.Id |> dispatch ) ]
          label
            [ ]
            [ str todo.Description ]
          button
            [ ClassName "destroy"
              OnClick (fun _ -> Destroy todo.Id |> dispatch)]
            [ ] ]
      input
        [ ClassName "edit"
          DefaultValue todo.Description
          Name "title"
          Id ("todo-" + (string todo.Id)) ]
    ]


let viewTodos model dispatch =
    let todos = model.Todos
    let cssVisibility =
        if List.isEmpty todos then "hidden" else "visible"

    section
      [ ClassName "main"
        Style [ Visibility cssVisibility ]]
      [ ul
          [ ClassName "todo-list" ]
          (todos
           |> List.map (fun i -> viewTodo i dispatch)) ]

let view model dispatch =
  div
    [ ClassName "todomvc-wrapper"]
    [ section
        [ ClassName "todoapp" ]
        [ viewInput model dispatch
          viewTodos model dispatch ] ]

// Main

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

let save (todos: Todo list) =
    promise {
        let! _ = Fetch.post<Todo list,string>(Url.todos, todos)
        return ()
    }

let updateAndSave msg model =
    let newModel = update msg model
    if newModel.Todos <> model.Todos then
        save newModel.Todos |> Promise.start

    newModel

let run todos =
    Program.mkSimple init updateAndSave view
    #if DEBUG
    |> Program.withConsoleTrace
    #endif
    |> Program.withReactBatched "elmish-app"
    #if DEBUG
    |> Program.withDebugger
    #endif
    |> Program.runWith todos

let loadAndRun () =
    promise {
        let! todos = Fetch.fetchAs<Todo list>(Url.todos)
        run todos
    }

loadAndRun() |> Promise.start