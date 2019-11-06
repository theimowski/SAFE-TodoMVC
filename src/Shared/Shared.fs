namespace Shared

open System

type Counter = { Value : int }

type Todo =
    { Id : Guid
      Description : string
      IsCompleted : bool }
