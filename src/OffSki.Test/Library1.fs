module ``storing a holiday``

open System
open System.Text.RegularExpressions
open Chronic

type Command =
  | Add 
  | Unknown of string

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
   
let parseSlot = function
    | [|from|] -> Some { When = from |> dateTimeToDate ; Days = 1 }
    | [|from ; to'|] -> Some { When = from |> dateTimeToDate ; Days = (to' - from).TotalDays + 1. |> int }
    | _ -> None

let parseDates = split "-" >> Array.map (textToSpan >> spanToDateTime) >> Array.choose id

let toOption none = function
    | x when x = none -> None
    | x -> Some x

let arrayToOption = toOption [||]

let stringToOption = toOption ""

let parseMessage text = 
    let parts = Regex.Match(text, "(?<command>\w+)(?<date>[^#]*)#?(?<note>.*)").Groups

    let command = parts.["command"].Value |> parseCommand
    let slot = parts.["date"].Value |> stringToOption |> Option.bind (parseDates >> arrayToOption) |> Option.bind parseSlot
    let note = parts.["note"].Value |> stringToOption
        
    (command, slot, note)

[<Test>]
let ``add tomorrow with note``() = parseMessage "add tomorrow #hospital" == (Add, Some tomorrow, Some "hospital")

[<Test>]
let ``add tomorrow without note``() = parseMessage "add tomorrow" == (Add, Some tomorrow, Option<string>.None)

[<Test>]
let ``add today with note``() = parseMessage "add today #hospital" == (Add, Some today, Some "hospital")

[<Test>]
let ``add with complex date``() = 
    let thirdJulySlot = createDate 2015 7 3 |> createSlot 1 |> Some
    parseMessage "add 3rd July" == (Add, thirdJulySlot, Option<string>.None)

[<Test>]
let ``add with date range``() = 
    let thirdJulySlotToFifth = createDate 2015 7 3 |> createSlot 3 |> Some
    parseMessage "add 3rd July - 5th July" == (Add, thirdJulySlotToFifth, Option<string>.None)

[<Test>]
let ``add with date range with note``() = 
    let thirdJulySlotToFifth = createDate 2015 7 3 |> createSlot 3 |> Some
    parseMessage "add 3rd July - 5th July #Magaloof" == (Add, thirdJulySlotToFifth, Some "Magaloof")

[<Test>]
let ``add with date range with year and note``() = 
    let thirdJulySlotToFifth = createDate 2016 7 3 |> createSlot 3 |> Some
    parseMessage "add 3rd July 2016 - 5th July 2016 #Magaloof" == (Add, thirdJulySlotToFifth, Some "Magaloof")

[<Test>]
let ``unknown command``() =
    parseMessage "boom" == (Unknown "boom", Option<Slot>.None, Option<string>.None)
  