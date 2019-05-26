namespace Shared

// this was moved from Server.fs
// now it can be used both for Client and Server

type Todo =
    { Id : int
      Description : string
      IsCompleted : bool }
