namespace Shared

open System

type Command = unit

type Todo =
    { Id : Guid
      Title : string
      Completed : bool }

type Event = unit

type Error = unit

module Todos =

    let handle (command: Command) (todos: Todo list) : Result<Event, Error> =
        match command with
        | () -> Ok ()

    let apply (event: Event) (todos: Todo list) =
        match event with
        | () -> todos
