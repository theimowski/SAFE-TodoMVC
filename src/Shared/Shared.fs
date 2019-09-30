namespace Shared

open System

type AddDTO =
    { Id : Guid
      Title : string }

type PatchSingleDTO =
    { Completed : bool option
      Title : string option }

type PatchAllDTO =
    { Completed : bool }

type Command =
    | Add of AddDTO
    | Patch of Guid * PatchSingleDTO
    | Delete of Guid
    | DeleteCompleted
    | PatchAll of PatchAllDTO

type Todo =
    { Id : Guid
      Title : string
      Completed : bool }

type Event =
    | Added of Todo
    | Patched of Todo
    | Deleted of Todo
    | CompletedDeleted
    | AllMarkedAs of bool

type Error =
    | DuplicateTodoId
    | TitleCannotBeEmpty
    | TodoNotFound

module Result =
    let ofOption e = function Some x -> Ok x | None -> Error e

module Todos =

    let patch (patchDTO: PatchSingleDTO) (todo: Todo) : Todo =
        { todo with
            Completed = patchDTO.Completed |> Option.defaultValue todo.Completed
            Title = patchDTO.Title |> Option.defaultValue todo.Title }

    let handle (command: Command) (todos: Todo list) : Result<Event, Error> =
        match command with
        | Add addDTO ->
            let exists = todos |> List.exists (fun t -> t.Id = addDTO.Id)
            if exists then Error DuplicateTodoId
            elif addDTO.Title = "" then Error TitleCannotBeEmpty
            else
                let todo : Todo =
                    { Id = addDTO.Id
                      Title = addDTO.Title
                      Completed = false }
                Added todo
                |> Ok
        | Patch (id, patchDTO) ->
            if patchDTO.Title |> Option.exists (fun t -> t = "") then
                Error TitleCannotBeEmpty
            else
                match todos |> List.tryFind (fun t -> t.Id = id) with
                | Some todo -> patch patchDTO todo |> Patched |> Ok
                | None -> Error TodoNotFound
        | Delete id ->
            match todos |> List.tryFind (fun t -> t.Id = id) with
            | Some todo -> Deleted todo |> Ok
            | None -> Error TodoNotFound
        | DeleteCompleted ->
            Ok CompletedDeleted
        | PatchAll patchDTO ->
            AllMarkedAs patchDTO.Completed
            |> Ok

    let apply (event: Event) (todos: Todo list) =
        match event with
        | Added todo ->
            todos @ [ todo ]
        | Patched todo ->
            List.map (fun t -> if t.Id = todo.Id then todo else t) todos
        | Deleted todo ->
            List.filter (fun t -> t.Id <> todo.Id) todos
        | CompletedDeleted ->
            List.filter (fun t -> not t.Completed) todos
        | AllMarkedAs completed ->
            List.map (fun t -> { t with Completed = completed }) todos
