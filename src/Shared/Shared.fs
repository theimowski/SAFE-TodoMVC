namespace Shared

open System

type AddDTO =
    { Id : Guid
      Title : string }

type PatchDTO =
    { Completed : bool }

type Command =
    | AddCommand of AddDTO
    | DeleteCommand of Guid
    | PatchCommand of Guid * PatchDTO
    | DeleteCompletedCommand

type Todo =
    { Id : Guid
      Title : string
      Completed : bool }

type Event =
    | TodoAdded of Todo
    | TodoDeleted of Todo
    | TodoPatched of Todo
    | CompletedTodosDeleted

type Error =
    | TodoIdAlreadyExists
    | TodoNotFound

module Todos =

    let handle (command: Command) (todos: Todo list) : Result<Event, Error> =
        match command with
        | AddCommand addDTO ->
            if todos |> List.exists (fun t -> t.Id = addDTO.Id) then
                Error TodoIdAlreadyExists
            else
                let todo : Todo =
                    { Id = addDTO.Id
                      Title = addDTO.Title
                      Completed = false }
                TodoAdded todo |> Ok
        | DeleteCommand id ->
            match todos |> List.tryFind (fun t -> t.Id = id) with
            | Some todo ->
                TodoDeleted todo |> Ok
            | None ->
                Error TodoNotFound
        | PatchCommand(id, patchDTO) ->
            match todos |> List.tryFind (fun t -> t.Id = id) with
            | Some todo ->
                { todo with Completed = patchDTO.Completed} |> TodoPatched |> Ok
            | None ->
                Error TodoNotFound
        | DeleteCompletedCommand ->
            CompletedTodosDeleted |> Ok

    let apply (event: Event) (todos: Todo list) =
        match event with
        | TodoAdded todo ->
            todos @ [ todo ]
        | TodoDeleted todo ->
            todos |> List.filter (fun t -> t.Id <> todo.Id)
        | TodoPatched todo ->
            todos |> List.map (fun t -> if t.Id = todo.Id then todo else t)
        | CompletedTodosDeleted ->
            todos |> List.filter (fun t -> not t.Completed)
