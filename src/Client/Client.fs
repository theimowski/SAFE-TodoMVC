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
      Input : string
      Editing : (Guid * string) option }

// Messages

type Msg =
    | TodosFetched of Todo list
    | ExecuteCommand of Command
    | EventApplied of Todo list
    | UpdateInput of string
    | AddTodo
    | SetCompleted of Guid * bool
    | Destroy of Guid
    | ClearCompleted
    | SetAll of bool
    | StartEditing of Guid
    | UpdateEditing of string
    | SaveEdit
    | AbortEdit

// Fetch

let fetchTodos () = Fetch.fetchAs<Todo list>(Url.todos)

let request (command: Command) =
    match command with
    | Add addDTO ->
        Fetch.post<AddDTO,Todo list>(Url.todos, addDTO)
    | Patch (id, patchDTO) ->
        Fetch.patch<PatchSingleDTO,Todo list>(Url.todo (string id), patchDTO)
    | Delete id ->
        Fetch.delete(Url.todo (string id), "")
    | DeleteCompleted ->
        Fetch.delete(Url.todos, "")
    | PatchAll patchDTO ->
        Fetch.patch<PatchAllDTO,Todo list>(Url.todos, patchDTO)

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
    | AddTodo ->
        let addDTO : AddDTO =
            { Id = Guid.NewGuid()
              Title = model.Input }
        let cmd = Add addDTO |> execute
        { model with Input = "" }, cmd
    | SetCompleted (id, completed) ->
        let patchDTO : PatchSingleDTO =
            { Completed = Some completed
              Title = None }
        let cmd = Patch (id, patchDTO) |> execute
        model, cmd
    | Destroy id ->
        let cmd = Delete id |> execute
        model, cmd
    | ClearCompleted ->
        let cmd = DeleteCompleted |> execute
        model, cmd
    | SetAll completed ->
        let patchDTO : PatchAllDTO =
            { Completed = completed }
        let cmd = PatchAll patchDTO |> execute
        model, cmd
    | StartEditing id ->
        let editing =
            model.Todos
            |> List.tryFind (fun t -> t.Id = id)
            |> Option.map (fun t -> t.Id, t.Title)
        { model with Editing = editing }, Cmd.none
    | SaveEdit ->
        let cmd =
            match model.Editing with
            | Some (id, title) ->
                let patchDTO : PatchSingleDTO =
                    { Completed = None
                      Title = Some title }
                Patch (id, patchDTO) |> execute
            | None -> Cmd.none
        { model with Editing = None }, cmd
    | AbortEdit ->
        { model with Editing = None }, Cmd.none
    | UpdateEditing value ->
        let editing =
            model.Editing
            |> Option.map (fun (id,_) -> id, value)
        { model with Editing = editing }, Cmd.none

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

let viewTodo (todo, editing: string option) dispatch =
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
              OnChange (fun _ -> SetCompleted(todo.Id, not todo.Completed) |> dispatch) ]
          label
            [ OnDoubleClick (fun _ -> StartEditing todo.Id |> dispatch) ]
            [ str todo.Title ]
          button
            [ ClassName "destroy"
              OnClick (fun _ -> Destroy todo.Id |> dispatch ) ]
            [ ] ]
      input
        [ ClassName "edit"
          valueOrDefault (editing |> Option.defaultValue todo.Title)
          OnChange (fun e -> e.target?value |> UpdateEditing |> dispatch)
          Name "title"
          OnKeyDown
            (fun e ->
                if e.keyCode = 13. then dispatch SaveEdit
                elif e.keyCode = 27. then dispatch AbortEdit) ] ]

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
           |> List.map (fun todo ->
                let editing =
                    model.Editing
                    |> Option.bind (fun (id,editing) -> if id = todo.Id then Some editing else None)
                viewTodo (todo, editing) dispatch)) ]

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