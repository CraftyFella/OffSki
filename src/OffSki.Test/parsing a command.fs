module ``parsing add command``

open System
open System.Text.RegularExpressions
open Chronic
open Parser

let createSlot (days : int) (from : Date) = 
    { When = from; Days = days }

let createDay = dateTimeToDate >> createSlot 1

let today = createDay DateTime.Today

let tomorrow = DateTime.Today.AddDays 1. |> createDay

[<Test>]
let ``add tomorrow with note``() = parseAdd "add tomorrow #hospital" == (Add, Some tomorrow, Some "hospital")

[<Test>]
let ``add tomorrow without note``() = parseAdd "add tomorrow" == (Add, Some tomorrow, Option<string>.None)

[<Test>]
let ``add today with note``() = parseAdd "add today #hospital" == (Add, Some today, Some "hospital")

[<Test>]
let ``add with complex date``() = 
    let thirdJulySlot = createDate 2015 7 3 |> createSlot 1 |> Some
    parseAdd "add 3rd July" == (Add, thirdJulySlot, Option<string>.None)

[<Test>]
let ``add with date range``() = 
    let thirdJulySlotToFifth = createDate 2015 7 3 |> createSlot 3 |> Some
    parseAdd "add 3rd July - 5th July" == (Add, thirdJulySlotToFifth, Option<string>.None)

[<Test>]
let ``add with date range with note``() = 
    let thirdJulySlotToFifth = createDate 2015 7 3 |> createSlot 3 |> Some
    parseAdd "add 3rd July - 5th July #Magaloof" == (Add, thirdJulySlotToFifth, Some "Magaloof")

[<Test>]
let ``add with date range with year and note``() = 
    let thirdJulySlotToFifth = createDate 2016 7 3 |> createSlot 3 |> Some
    parseAdd "add 3rd July 2016 - 5th July 2016 #Magaloof" == (Add, thirdJulySlotToFifth, Some "Magaloof")

[<Test>]
let ``unknown command``() =
    parseAdd "boom" == (Unknown "boom", Option<Slot>.None, Option<string>.None)