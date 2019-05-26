namespace Shared

// Moved Todo from Server to Shared
// Now it is accessible from Server and Client

type Todo =
    { Id : int
      Description : string
      IsCompleted : bool }