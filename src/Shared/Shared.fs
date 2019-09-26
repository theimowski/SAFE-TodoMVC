namespace Shared

open System

type AddDTO =
    { Id : Guid
      Title : string }

type PatchDTO =
    { Completed : bool }

type Command =
    | Add of AddDTO
    | Patch of Guid * PatchDTO

type Todo =
    { Id : Guid
      Title : string
      Completed : bool }

type Event =
    | Added of Todo
    | Patched of Todo

module Todos =

    let patch (patchDTO: PatchDTO) (todo: Todo) : Todo =
        { todo with Completed = patchDTO.Completed }

    let handle (command: Command) (todos: Todo list) : Event =
        match command with
        | Add addDTO ->
            let todo : Todo =
                { Id = addDTO.Id
                  Title = addDTO.Title
                  Completed = false }
            Added todo
        | Patch (id, patchDTO) ->
            todos
            |> List.find (fun t -> t.Id = id)
            |> patch patchDTO
            |> Patched

    let apply (event: Event) (todos: Todo list) =
        match event with
        | Added todo ->
            todos @ [ todo ]
        | Patched todo ->
            List.map (fun t -> if t.Id = todo.Id then todo else t) todos

module Url =
    let todos = "/api/init"
