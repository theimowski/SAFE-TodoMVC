namespace Shared


// MODEL
type Entry =
    { description : string
      completed : bool
      id : int }

// The full application state of our todo app.
type Model =
    { entries : Entry list
      editing : int option
      field : string
      uid : int
      visibility : string }

