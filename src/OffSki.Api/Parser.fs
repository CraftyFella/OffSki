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

let datesToSlot (dateTimeToDate : DateTime -> Date) =
  function
  | Some [| from |] ->
    Some { Slot.When = from |> dateTimeToDate
           Days = 1 }
  | Some [| from; to' |] ->
    Some { Slot.When = from |> dateTimeToDate
           Days = (to' - from).TotalDays + 1. |> int }
  | _ -> None

let stringToDates =
  split "-"
  >> Array.map (textToSpan >> spanToDateTime)
  >> Array.choose id

let stringToSlot stringToDates datesToSlot  =
   Option.lift ""
  >> Option.bind (stringToDates >> Option.lift [||])
  >> datesToSlot

let parseAdd stringToSlot command =
  let options =
    match command with
    | Add(Options.Unknown options) -> options

  let parts = Regex.Match(options, "(?<date>[^#]*)#?(?<note>.*)").Groups
  let slot = parts.["date"].Value |> stringToSlot
  let note = parts.["note"].Value |> Option.lift ""

  match slot, note with
    | Some s, Some n -> SlotWithNote (s, n) |> Add
    | Some s, None -> Slot s |> Add
    | _ -> Add Invalid

let handleCommand parseAdd =
  function
  | Add _ as command -> parseAdd command
  | Unknown message -> Unknown message

let datesToSlot' = datesToSlot dateTimeToDate
let stringToSlot' = stringToSlot stringToDates datesToSlot'
let parseAdd' = parseAdd stringToSlot'

let handleCommand' = handleCommand parseAdd'

let parseOffski = parseCommand >> handleCommand'