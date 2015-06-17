module ``storing a holiday``

open System
open System.Text.RegularExpressions
open Chronic

exception InvalidCommand
type Command = Add
let createCommand = function
    | "add" -> Add
    | _ -> raise InvalidCommand

type Date = { Year : int; Month : int; Day : int }
let createDate year month day = {Year = year ; Month = month ; Day = day}

type Slot = { When : Date; Days : int }
let createSlot days when' = {When = when'; Days = days} 

let toDate (s : DateTime) =
    { Year = s.Year; Month = s.Month; Day = s.Day }

let tomorrow = DateTime.Today.AddDays 1. |> toDate |> createSlot 1
let today = DateTime.Today |> toDate |> createSlot 1

let spanToSlot (s : Span) = 
    let days = (s.End.Value - s.Start.Value).TotalDays |> int
    s.Start.Value |> toDate |> createSlot days
    
let dateToSlot =
    let parser = Parser()
    parser.Parse >> spanToSlot
    
let parseMessage text = 
    let parts = Regex.Match(text, "(?<command>\w+)(?<date>[^#]*)#?(?<note>.*)").Groups
    let command = parts.["command"].Value |> createCommand
    let slot = parts.["date"].Value |> dateToSlot
    
    let note = 
        match parts.["note"].Value with
        | "" | null -> None
        | n -> Some n

    (command, slot, note)

[<Test>]
let ``add tomorrow with note``() = parseMessage "add tomorrow #hospital" == (Add, tomorrow, Some "hospital")

[<Test>]
let ``add tomorrow without note``() = parseMessage "add tomorrow" == (Add, tomorrow, Option<string>.None)

[<Test>]
let ``add today with note``() = parseMessage "add today #hospital" == (Add, today, Some "hospital")

[<Test>]
let ``add with complex date``() = 
    let thirdJulySlot = createDate 2015 7 3 |> createSlot 1
    parseMessage "add 3rd July" == (Add, thirdJulySlot, Option<string>.None)