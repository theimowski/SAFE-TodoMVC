module Client

open System

open Browser
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
    | EventApplied of Event
    | AddTodo
    | SetCompleted of Guid * bool

// Fetch

let fetchTodos () = Fetch.fetchAs<Todo list>(Url.todos)
let addTodo (addDTO) = Fetch.post<AddDTO,Event>(Url.todos, addDTO)
let patchTodo (id, patchDTO) =
    Fetch.patch<PatchDTO,Event>(Url.todo (string id), patchDTO)

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
    | EventApplied event ->
        console.log (sprintf "Event: %A" event)
        model, Cmd.none
    | AddTodo ->
        let addDTO =
            { Id = Guid.NewGuid()
              Title = model.Input }
        let event = Todos.handle (Add addDTO) model.Todos
        let todos = Todos.apply event model.Todos
        let cmd = Cmd.OfPromise.perform addTodo addDTO EventApplied
        { model with Input = ""; Todos = todos }, cmd
    | SetCompleted (id, completed) ->
        let patchDTO : PatchDTO =
            { Completed = completed }
        let event = Todos.handle (Patch (id, patchDTO)) model.Todos
        let todos = Todos.apply event model.Todos
        let cmd = Cmd.OfPromise.perform patchTodo (id, patchDTO) EventApplied
        { model with Todos = todos }, cmd


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
    [ classList [ ("completed", todo.Completed) ] ]
    [ div
        [ ClassName "view" ]
        [ input
            [ Type "checkbox"
              ClassName "toggle"
              Checked todo.Completed
              OnChange (fun _ -> SetCompleted(todo.Id, not todo.Completed) |> dispatch) ]
          label
            [ ]
            [ str todo.Title ] ] ]

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