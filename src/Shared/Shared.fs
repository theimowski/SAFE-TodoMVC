namespace Shared

type Todo =
    { Id : int
      Description : string
      IsCompleted : bool }

module Url =
    let todos = "/api/todos"