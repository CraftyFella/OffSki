module Parser    

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

let parseCommand = 
    function 
    | "add" -> Add
    | command -> Unknown command

let dateTimeToDate (d : DateTime) = 
    createDate d.Year d.Month d.Day
   
let datesToSlot = function
    | [|from|] -> Some { When = from |> dateTimeToDate ; Days = 1 }
    | [|from ; to'|] -> Some { When = from |> dateTimeToDate ; Days = (to' - from).TotalDays + 1. |> int }
    | _ -> None

let parseDates = split "-" >> Array.map (textToSpan >> spanToDateTime) >> Array.choose id

let toOption none = function
    | x when x = none -> None
    | x -> Some x

let arrayToOption = toOption [||]

let stringToOption = toOption ""

let stringToSlot = stringToOption >> Option.bind (parseDates >> arrayToOption) >> Option.bind datesToSlot

let parseAdd text = 
    let parts = Regex.Match(text, "(?<command>\w+)(?<date>[^#]*)#?(?<note>.*)").Groups

    let command = parts.["command"].Value |> parseCommand
    let slot = parts.["date"].Value |> stringToSlot
    let note = parts.["note"].Value |> stringToOption
        
    (command, slot, note)
