namespace Shared


// MODEL
type Entry =
    { description : string
      completed : bool
      editing : bool
      id : int }

// The full application state of our todo app.
type Model =
    { entries : Entry list
      field : string
      uid : int
      visibility : string }

