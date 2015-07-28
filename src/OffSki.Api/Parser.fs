module Parser

#nowarn "25"

open Chronic
open System
open System.Text.RegularExpressions

let spanToDateTime (s : Span) =
  if s.Start.HasValue then Some s.Start.Value
  else None

let textToSpan =
  let parser = Parser()
  parser.Parse

let split (c : string) (text : string) = text.Split([| c |], StringSplitOptions.RemoveEmptyEntries)

let parseCommand message =
  let command, options =
    message
    |> split " "
    |> Array.toList
    |> function
    | command :: options ->
      command,
      (options
       |> String.concat " "
       |> Options.Unknown)
  match command.ToLower() with
  | "add" -> Add options
  | "list" -> List options
  | _ -> Unknown message

let dateTimeToDate (d : DateTime) = createDate d.Year d.Month d.Day

let datesToSlot =
  function
  | [| from |] ->
    Some { Slot.When = from |> dateTimeToDate
           Days = 1 }
  | [| from; to' |] ->
    Some { Slot.When = from |> dateTimeToDate
           Days = (to' - from).TotalDays + 1. |> int }
  | _ -> None

let parseDates =
  split "-"
  >> Array.map (textToSpan >> spanToDateTime)
  >> Array.choose id

let toOption none =
  function
  | x when x = none -> None
  | x -> Some x

let arrayToOption = toOption [||]
let stringToOption = toOption ""

let stringToSlot =
  stringToOption
  >> Option.bind (parseDates >> arrayToOption)
  >> Option.bind datesToSlot

let parseAdd command =
  let options =
    match command with
    | Add(Options.Unknown options) -> options

  let parts = Regex.Match(options, "(?<date>[^#]*)#?(?<note>.*)").Groups
  let slot = parts.["date"].Value |> stringToSlot
  let note = parts.["note"].Value |> stringToOption

  match (slot, note) with
    | Some s, Some n -> SlotWithNote (s, n) |> Add
    | Some s, None -> Slot s |> Add
    | _ -> Add Invalid

let handleCommand =
  function
  | Add _ as command -> parseAdd command
  | Unknown message -> Unknown message 

let parseOffski = parseCommand >> handleCommand