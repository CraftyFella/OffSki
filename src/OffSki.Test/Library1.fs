﻿module ``storing a holiday``

open System
open System.Text.RegularExpressions
open Chronic

exception InvalidCommand

type Command = Add

type Date = 
    { Year : int
      Month : int
      Day : int }

let createDate year month day = 
    { Year = year
      Month = month
      Day = day }

type Slot = 
    { When : Date
      Days : int }

let createSlot (days : int) (from : Date) = 
    { When = from; Days = days }

let dateTimeToDate (d : DateTime) = 
    createDate d.Year d.Month d.Day

let tomorrow = 
    DateTime.Today.AddDays 1.
    |> dateTimeToDate
    |> createSlot 1

let today = 
    DateTime.Today
    |> dateTimeToDate
    |> createSlot 1

let spanToDate (s : Span) = s.Start.Value |> dateTimeToDate

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
    | _ -> raise InvalidCommand

let parseSlot text = 

    let dates = text |> split " - "

    let from = 
        dates.[0]
        |> textToSpan
        |> spanToDateTime
    
    let to' = 
        if dates.Length = 1 then
            None 
        else 
            dates.[1] |> textToSpan |> spanToDateTime
    
    let days = 
        match to' with
        | Some d -> (d - from.Value).TotalDays |> int |> (+) 1
        | None -> 1

    { When = from.Value |> dateTimeToDate; Days = days }

let parseNote text =
    match text with
        | "" | null -> None
        | n -> Some n

let parseMessage text = 
    let parts = Regex.Match(text, "(?<command>\w+)(?<date>[^#]*)#?(?<note>.*)").Groups

    let command = parts.["command"].Value |> parseCommand
    let slot = parts.["date"].Value |> parseSlot
    let note = parts.["note"].Value |> parseNote
        
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

[<Test>]
let ``add with date range``() = 
    let thirdJulySlotToFifth = createDate 2015 7 3 |> createSlot 3
    parseMessage "add 3rd July - 5th July" == (Add, thirdJulySlotToFifth, Option<string>.None)

[<Test>]
let ``add with date range with note``() = 
    let thirdJulySlotToFifth = createDate 2015 7 3 |> createSlot 3
    parseMessage "add 3rd July - 5th July #Magaloof" == (Add, thirdJulySlotToFifth, Some "Magaloof")