module Client

open System

open Browser
open Browser.Types
open Elmish
open Elmish.React
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Fetch
open Thoth.Fetch
open Thoth.Json

open Shared

// Model

type Model =
    { Todos : Todo list
      Input : string }

// Messages

type Msg =
    | TodosFetched of Todo list
    | ExecuteCommand of Command
    | EventApplied of Todo list
    | UpdateInput of string
    | Add
    | Destroy of Guid
    | SetCompleted of Guid * bool
    | ClearCompleted

// Fetch

let todos = "/api/todos"
let todo (id: Guid) = sprintf "/api/todo/%O" id

let fetch method url (body: 'a option) =
    let properties =
        [ yield Method method
          match body with
          | Some body ->
            yield requestHeaders [ ContentType "application/json" ]
            yield Body (body |> Encode.toString 0 |> (!^))
          | None -> () ]
    Fetch.fetchAs<Todo list>(url, properties)

let fetchTodos () = fetch HttpMethod.GET todos None

let request (command: Command) =
    match command with
    | AddCommand addDTO -> fetch HttpMethod.POST todos (Some addDTO)
    | DeleteCommand id -> fetch HttpMethod.DELETE (todo id) None
    | PatchCommand(id, patchDTO) -> fetch HttpMethod.PATCH (todo id) (Some patchDTO)
    | DeleteCompletedCommand -> fetch HttpMethod.DELETE todos None

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
        match Todos.handle command model.Todos with
        | Ok event ->
            let todos = Todos.apply event model.Todos
            let cmd =
                Cmd.OfPromise.perform (fun _ -> request command) () EventApplied
            { model with Todos = todos }, cmd
        | Error e ->
            console.log (sprintf "Domain error: %A" e)
            model, Cmd.none
    | EventApplied todos ->
        console.log (sprintf "Todos in sync: %b" (todos = model.Todos))
        model, Cmd.none
    | Add ->
        let addDTO : AddDTO =
            { Id = Guid.NewGuid()
              Title = model.Input }
        let cmd = execute (AddCommand addDTO)
        { model with Input = "" }, cmd
    | Destroy id ->
        let cmd = execute (DeleteCommand id)
        model, cmd
    | SetCompleted (id, completed) ->
        let patchDTO : PatchDTO =
            { Completed = completed }
        let cmd = PatchCommand (id, patchDTO) |> execute
        model, cmd
    | ClearCompleted ->
        let cmd = execute DeleteCompletedCommand
        model, cmd


// View

module Key =
    let enter = 13.
    let esc = 27.

let viewInput (model: Model) dispatch =
    header [ ClassName "header" ]
        [ h1 [ ] [ str "todos" ]
          input
              [ ClassName "new-todo"
                Placeholder "What needs to be done?"
                OnChange (fun e -> e.target?value |> UpdateInput |> dispatch)
                OnKeyDown
                    (fun e -> if e.keyCode = Key.enter then dispatch Add)
                valueOrDefault model.Input
                AutoFocus true ] ]

let viewTodo (todo: Todo) dispatch =
  li
    [ classList
        [ "completed", todo.Completed ] ]
    [ div
        [ ClassName "view" ]
        [ input
            [ Type "checkbox"
              ClassName "toggle"
              Checked todo.Completed
              OnChange (fun _ -> SetCompleted (todo.Id, not todo.Completed) |> dispatch) ]
          label
            [ ]
            [ str todo.Title ]
          button
            [ ClassName "destroy"
              OnClick (fun _ -> Destroy todo.Id |> dispatch) ]
            [ ] ] ]

let viewTodos model dispatch =
    let todos = model.Todos
    let cssVisibility =
        if List.isEmpty todos then "hidden" else "visible"

    section
      [ ClassName "main"
        Style [ Visibility cssVisibility ]]
      [ ul
          [ ClassName "todo-list" ]
          [ for todo in todos ->
                viewTodo todo dispatch ] ]

let viewControlsCount todosLeft =
    let item =
        if todosLeft = 1 then " item" else " items"

    span
        [ ClassName "todo-count" ]
        [ strong [] [ str (string todosLeft) ]
          str (item + " left") ]

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

    let todosLeft = model.Todos.Length - todosCompleted

    footer
        [ ClassName "footer"
          Hidden (List.isEmpty model.Todos) ]
        [ viewControlsCount todosLeft
          viewControlsClear todosCompleted dispatch ]

let view model dispatch =
    div
      [ ClassName "todomvc-wrapper"]
      [ section
          [ ClassName "todoapp" ]
          [ viewInput model dispatch
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