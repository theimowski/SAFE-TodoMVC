namespace Shared

open System

type Todo =
    { Id : Guid
      Description : string
      IsCompleted : bool }

module Url =
    let todos = "/api/init"
