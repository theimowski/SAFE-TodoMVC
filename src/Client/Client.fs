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
    | ExecuteCommand of Command
    | EventApplied of Event
    | UpdateInput of string
    | AddTodo
    | SetCompleted of Guid * bool
    | Destroy of Guid
    | ClearCompleted
    | SetAll of bool

// Fetch

let fetchTodos () = Fetch.fetchAs<Todo list>(Url.todos)

let request (command: Command) =
    match command with
    | Add addDTO ->
        Fetch.post<AddDTO,Event>(Url.todos, addDTO)
    | Patch (id, patchDTO) ->
        Fetch.patch<PatchDTO,Event>(Url.todo (string id), patchDTO)
    | Delete id ->
        Fetch.delete(Url.todo (string id), "")
    | DeleteCompleted ->
        Fetch.delete(Url.todosCompleted, "")
    | PatchAll patchDTO ->
        Fetch.patch<PatchDTO,Event>(Url.todos, patchDTO)

// Initial model and command

let init () : Model * Cmd<Msg> =
    let cmd = Cmd.OfPromise.perform fetchTodos () TodosFetched
    let model =
        { Todos = []
          Input = "" }
    model, cmd

// Update

let execute = ExecuteCommand >> Cmd.ofMsg

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | TodosFetched todos ->
        { model with Todos = todos }, Cmd.none
    | UpdateInput value ->
        { model with Input = value }, Cmd.none
    | ExecuteCommand command ->
        let event = Todos.handle command model.Todos
        let todos = Todos.apply event model.Todos
        let cmd =
            Cmd.OfPromise.perform (fun _ -> request command) () EventApplied
        { model with Todos = todos }, cmd
    | EventApplied event ->
        console.log (sprintf "Event: %A" event)
        model, Cmd.none
    | AddTodo ->
        let addDTO : AddDTO =
            { Id = Guid.NewGuid()
              Title = model.Input }
        let cmd = Add addDTO |> execute
        { model with Input = "" }, cmd
    | SetCompleted (id, completed) ->
        let patchDTO : PatchDTO =
            { Completed = completed }
        let cmd = Patch (id, patchDTO) |> execute
        model, cmd
    | Destroy id ->
        let cmd = Delete id |> execute
        model, cmd
    | ClearCompleted ->
        let cmd = DeleteCompleted |> execute
        model, cmd
    | SetAll completed ->
        let patchDTO : PatchDTO =
            { Completed = completed }
        let cmd = PatchAll patchDTO |> execute
        model, cmd

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
            [ str todo.Title ]
          button
            [ ClassName "destroy"
              OnClick (fun _ -> Destroy todo.Id |> dispatch ) ]
            [ ] ] ]

let viewTodos model dispatch =
    let todos = model.Todos
    let cssVisibility =
        if List.isEmpty todos then "hidden" else "visible"

    let allCompleted =
        todos
        |> List.forall (fun t -> t.Completed)

    section
      [ ClassName "main"
        Style [ Visibility cssVisibility ]]
      [ input
          [ ClassName "toggle-all"
            Type "checkbox"
            Name "toggle"
            Checked allCompleted
            OnChange ignore ]
        label
          [ HtmlFor "toggle-all"
            OnClick (fun _ -> SetAll (not allCompleted) |> dispatch)]
          [ str "Mark all as complete" ]
        ul
          [ ClassName "todo-list" ]
          (todos
           |> List.map (fun todo -> viewTodo todo dispatch)) ]

let viewControlsClear todosCompleted dispatch =
  button
    [ ClassName "clear-completed"
      Hidden (todosCompleted = 0)
      OnClick (fun _ -> ClearCompleted |> dispatch) ]
    [ str ("Clear completed") ]

let viewControls model dispatch =
  let todosCompleted =
      model.Todos
      |> List.filter (fun t -> t.Completed)
      |> List.length

  footer
      [ ClassName "footer"
        Hidden (List.isEmpty model.Todos) ]
      [ viewControlsClear todosCompleted dispatch ]

let view model dispatch =
  div
    [ ClassName "todomvc-wrapper"]
    [ section
        [ ClassName "todoapp" ]
        [ viewInput model.Input dispatch
          viewTodos model dispatch
          viewControls model dispatch ] ]

// Main

open Elmish.Debug
open Elmish.HMR

Program.mkProgram init update view
|> Program.withConsoleTrace
|> Program.withReactBatched "elmish-app"
|> Program.withDebugger
|> Program.run