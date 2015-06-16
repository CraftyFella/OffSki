module ``storing a holiday``

open Messages
open System

open System.Text.RegularExpressions
open Chronic

type Date = { Year : int; Month : int; Day : int }
type Slot = { Start : Date; End : Date }

let toDate (s : DateTime) =
    { Year = s.Year; Month = s.Month; Day = s.Day }

let toSlot (start : DateTime, finish : DateTime) = 
    let startDate = start |> toDate
    let finishDate = finish |> toDate
    { Start = startDate;
     End = finishDate }

let singleToSlot (start : DateTime) = 
    toSlot(start, start.AddDays 1.)

let spanToSlot (s : Span) = 
    let start = s.Start.Value
    let finish = s.End.Value
    toSlot(start, finish)

let tomorrow = DateTime.Today.AddDays(1.) |> singleToSlot
let today = DateTime.Today |> singleToSlot

type Command = 
    | AddHoliday of Slot * string option
    
let parseDate text =
    let parser = Parser()
    parser.Parse text
    
let parseMessage text = 
    let parts = Regex.Match(text, "(?<command>\w+)(?<date>[^#]*)#?(?<note>.*)").Groups
    let command = parts.["command"].Value
    let slot = parts.["date"].Value |> parseDate |> spanToSlot
    
    let note = 
        match parts.["note"].Value with
        | "" | null -> None
        | n -> Some n

    AddHoliday(slot, note)

[<Test>]
let ``add with note``() = parseMessage "add tomorrow #hospital" == AddHoliday(tomorrow, Some "hospital")

[<Test>]
let ``add without note``() = parseMessage "add tomorrow" == AddHoliday(tomorrow, None)

[<Test>]
let ``add with complex date``() = 
    let thirdJuly = new DateTime(2015, 7, 3) |> singleToSlot
    parseMessage "add 3rd July" == AddHoliday(thirdJuly, None)
