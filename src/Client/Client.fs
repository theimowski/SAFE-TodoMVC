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

// Messages

type Msg =
    | TodosFetched of Todo list
    | UpdateInput of string
    | AddTodo
    | TodoAdded of Todo
    | Toggle of Guid

// Fetch

let fetchTodos () = Fetch.fetchAs<Todo list>(Url.todos)
let addTodo todo = Fetch.post<Todo, Todo>(Url.todos, todo)

// Initial model and command

let init () : Model * Cmd<Msg> =
    let cmd = Cmd.OfPromise.perform fetchTodos () TodosFetched
    let model =
        { Todos = []
          Input = "" }
    model, cmd

// Update

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | TodosFetched todos ->
        { model with Todos = todos }, Cmd.none
    | UpdateInput value ->
        { model with Input = value }, Cmd.none
    | AddTodo ->
        let newTodo =
            { Id = Guid.NewGuid()
              Description = model.Input
              IsCompleted = false }
        let cmd = Cmd.OfPromise.perform addTodo newTodo TodoAdded
        { model with
            Input = ""
            Todos = model.Todos @ [ newTodo ]}, cmd
    | TodoAdded todo ->
        model, Cmd.none
    | Toggle id ->
        let toggle (todo: Todo) =
            if todo.Id <> id then todo
            else { todo with IsCompleted = not todo.IsCompleted }
        { model with Todos = List.map toggle model.Todos }, Cmd.none

// View

let viewInput (model:string) dispatch =
    header [ ClassName "header" ] [
        h1 [] [ str "todos" ]
        input [
            ClassName "new-todo"
            Placeholder "What needs to be done?"
            valueOrDefault model
            OnChange (fun e -> e.target?value |> UpdateInput |> dispatch)
            OnKeyDown (fun e -> if e.keyCode = 13. then dispatch AddTodo)
            AutoFocus true ] ]

let viewTodo (todo) dispatch =
  li
    [ classList [ ("completed", todo.IsCompleted) ] ]
    [ div
        [ ClassName "view" ]
        [ input
            [ Type "checkbox"
              ClassName "toggle"
              Checked todo.IsCompleted
              OnChange (fun e -> Toggle todo.Id |> dispatch) ]
          label
            [ ]
            [ str todo.Description ] ] ]

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
           |> List.map (fun todo -> viewTodo todo dispatch)) ]

let view model dispatch =
  div
    [ ClassName "todomvc-wrapper"]
    [ section
        [ ClassName "todoapp" ]
        [ viewInput model.Input dispatch
          viewTodos model dispatch ] ]

// Main

open Elmish.Debug
open Elmish.HMR

Program.mkProgram init update view
|> Program.withConsoleTrace
|> Program.withReactBatched "elmish-app"
|> Program.withDebugger
|> Program.run