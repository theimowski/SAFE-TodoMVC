module Client

open Browser.Types
open Elmish
open Elmish.React
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Thoth.Fetch

open Shared

// Todos type comes from Shared module
type Model =
  { Todos : Todo list
    Field : string
    NextId : int }

type Msg =
    | UpdateField of string
    | Add
    | Toggle of Todo
    | Destroy of Todo

let nextId (todos : Todo list) =
    if todos.Length = 0 then 0
    else
        todos
        |> List.map (fun todo -> todo.Id)
        |> List.max
        |> (+) 1

let init todos : Model =
    { Todos = todos
      Field = ""
      NextId = nextId todos }

let addTodo model =
    let newTodo =
      { Description = model.Field
        IsCompleted = false
        Id = model.NextId }
    List.append model.Todos [ newTodo ]

let update (msg : Msg) (model : Model) : Model =
    match msg with
    | UpdateField value ->
        { model with Field = value }
    | Add ->
        { NextId = model.NextId + 1
          Field = ""
          Todos = addTodo model }
    | Toggle todo ->
        let toggle t =
            if t.Id <> todo.Id then t
            else { t with IsCompleted = not t.IsCompleted }
        { model with Todos = List.map toggle model.Todos }
    | Destroy todo ->
        let predicate t = t.Id <> todo.Id
        { model with Todos = List.filter predicate model.Todos }

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

let viewTodo (todo) dispatch =
  li
    [ classList [ ("completed", todo.IsCompleted) ] ]
    [ div
        [ ClassName "view" ]
        [ input
            [ Type "checkbox"
              ClassName "toggle"
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
        [ viewInput model.Field dispatch
          viewTodos model dispatch ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

let load =
    promise {
        return! Fetch.fetchAs<Todo list> "/api/todos"
    }

let save (todos : Todo list) =
    promise {
        let! msg = Fetch.post ("/api/todos", todos)
        return ()
    }

let updateAndSave msg model =
    let newModel = update msg model
    if newModel.Todos <> model.Todos then
        Promise.start (save newModel.Todos)
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

load.``then``(fun todos -> run todos)
|> Promise.start