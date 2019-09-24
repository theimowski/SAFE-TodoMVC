module Client

open System

open Browser.Types
open Elmish
open Elmish.Debug
open Elmish.HMR
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

// Fetch

let fetchTodos () = Fetch.fetchAs<Todo list>("/api/todos")

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
        { model with
            Input = ""
            Todos = model.Todos @ [ newTodo ]}, Cmd.none

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
        [ label
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

Program.mkProgram init update view
|> Program.withConsoleTrace
|> Program.withReactBatched "elmish-app"
|> Program.withDebugger
|> Program.run