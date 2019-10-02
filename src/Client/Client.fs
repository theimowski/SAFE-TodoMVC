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
      Input : string
      Editing : (Guid * string) option }

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
    | SetAllCompleted of bool
    | StartEditing of Guid
    | AbortEditing
    | SetEditingValue of string
    | ApplyEditing

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
    | PatchAllCommand patchDTO -> fetch HttpMethod.PATCH todos (Some patchDTO)

// Initial model and command

let init () : Model * Cmd<Msg> =
    let cmd = Cmd.OfPromise.perform fetchTodos () TodosFetched
    let model =
        { Todos = []
          Input = ""
          Editing = None }
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
            { Completed = Some completed
              Title = None }
        let cmd = PatchCommand (id, patchDTO) |> execute
        model, cmd
    | ClearCompleted ->
        let cmd = execute DeleteCompletedCommand
        model, cmd
    | SetAllCompleted completed ->
        let patchDTO : PatchAllDTO =
            { Completed = completed }
        let cmd = PatchAllCommand patchDTO |> execute
        model, cmd
    | StartEditing id ->
        let editing =
            model.Todos
            |> List.tryFind (fun t -> t.Id = id)
            |> Option.map (fun todo -> todo.Id, todo.Title)
        { model with Editing = editing }, Cmd.none
    | AbortEditing ->
        { model with Editing = None }, Cmd.none
    | SetEditingValue value ->
        let editing =
            model.Editing
            |> Option.map (fun (id,_) -> id, value)
        { model with Editing = editing }, Cmd.none
    | ApplyEditing ->
        match model.Editing with
        | None -> model, Cmd.none
        | Some (id, value) ->
            let patchDTO : PatchDTO =
                { Completed = None
                  Title = Some value }
            let cmd = PatchCommand (id, patchDTO) |> execute
            { model with Editing = None }, cmd

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

let viewTodo (todo: Todo, editing: string option) dispatch =
  li
    [ classList
        [ "completed", todo.Completed
          "editing", Option.isSome editing ] ]
    [ div
        [ ClassName "view" ]
        [ input
            [ Type "checkbox"
              ClassName "toggle"
              Checked todo.Completed
              OnChange (fun _ -> SetCompleted (todo.Id, not todo.Completed) |> dispatch) ]
          label
            [ OnDoubleClick (fun _ -> StartEditing todo.Id |> dispatch) ]
            [ str todo.Title ]
          button
            [ ClassName "destroy"
              OnClick (fun _ -> Destroy todo.Id |> dispatch) ]
            [ ] ]
      input
        [ ClassName "edit"
          valueOrDefault (defaultArg editing "")
          OnChange
            (fun e -> e.target?value |> SetEditingValue |> dispatch)
          OnKeyDown
            (fun e ->
                if e.keyCode = Key.esc then dispatch AbortEditing
                elif e.keyCode = Key.enter then dispatch ApplyEditing) ] ]

let viewTodos model dispatch =
    let todos = model.Todos
    let cssVisibility =
        if List.isEmpty todos then "hidden" else "visible"

    let allCompleted =
        todos |> List.forall (fun t -> t.Completed)

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
            OnClick (fun _ -> SetAllCompleted (not allCompleted) |> dispatch) ]
          [ str "Mark all as complete" ]
        ul
          [ ClassName "todo-list" ]
          [ for todo in todos ->
                let editing =
                    model.Editing
                    |> Option.bind
                        (fun (id,title) ->
                            if id = todo.Id then Some title else None)
                viewTodo (todo, editing) dispatch ] ]

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