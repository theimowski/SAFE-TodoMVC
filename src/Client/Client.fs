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

type Todo =
    { Id : Guid
      Description : string
      IsCompleted : bool }

type Model =
    { Todos : Todo list
      Input : string
      NextId : Guid }

// Initial model

let init todos : Model =
    { Todos = todos
      Input = ""
      NextId = Guid.NewGuid() }

// Messages

type Msg =
    | UpdateField of string
    | Add

// Update model

let update (msg : Msg) (model : Model) : Model =
    match msg with
    | UpdateField value ->
        { model with Input = value }
    | Add ->
        let newTodo =
          { Description = model.Input
            IsCompleted = false
            Id = Guid.NewGuid() }
        { NextId = Guid.NewGuid()
          Input = ""
          Todos = List.append model.Todos [ newTodo ] }

// Helpers

let [<Literal>] ENTER_KEY = 13.

let internal onEnter msg dispatch =
    function
    | (ev:KeyboardEvent) when ev.keyCode = ENTER_KEY ->
        dispatch msg
    | _ -> ()
    |> OnKeyDown

// View (React)

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
        [ label
            [ ]
            [ str todo.Description ] ]
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
        [ viewInput model.Input dispatch
          viewTodos model dispatch ] ]

// Main

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

let updateAndSave msg model =
    let newModel = update msg model
    if newModel.Todos <> model.Todos then
        () // TODO: save model
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

run [ ]