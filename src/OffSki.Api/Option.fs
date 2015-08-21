module Option

let lift none =
  function
  | x when x = none -> None
  | x -> Some x