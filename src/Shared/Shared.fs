namespace Shared

open System

type AddDTO =
    { Id : Guid
      Title : string }

type Command =
    | Add of AddDTO

type Todo =
    { Id : Guid
      Title : string
      Completed : bool }

type Event =
    | Added of Todo

type Error =
    | DuplicateTodoId
    | TitleCannotBeEmpty

module Todos =

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

    let apply (event: Event) (todos: Todo list) =
        match event with
        | Added todo ->
            todos @ [ todo ]
