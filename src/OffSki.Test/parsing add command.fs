module ``parsing add command``

open System
open System.Text.RegularExpressions
open Chronic
open Parser

let createSlot (days : int) (from : Date) = 
  { When = from
    Days = days }

let createDay = dateTimeToDate >> createSlot 1
let today = DateTime.Today |> createDay
let tomorrow = DateTime.Today.AddDays 1. |> createDay

[<Test>]
let ``add tomorrow with note``() = parseOffski "add tomorrow #hospital" == Add(SlotWithNote(tomorrow, "hospital"))

[<Test>]
let ``add tomorrow without note``() = parseOffski "add tomorrow" == Add(Slot tomorrow)

[<Test>]
let ``add today with note``() = parseOffski "add today #hospital" == Add(SlotWithNote(today, "hospital"))

[<Test>]
let ``add with complex date``() = 
  let thirdJulySlot = createDate 2015 7 3 |> createSlot 1
  parseOffski "add 3rd July" == Add(Slot thirdJulySlot)

[<Test>]
let ``add with date range``() = 
  let thirdJulySlotToFifth = createDate 2015 7 3 |> createSlot 3
  parseOffski "add 3rd July - 5th July" == Add(Slot thirdJulySlotToFifth)

[<Test>]
let ``add with date range with note``() = 
  let thirdJulySlotToFifth = createDate 2015 7 3 |> createSlot 3
  parseOffski "add 3rd July - 5th July #Magaloof" == Add(SlotWithNote(thirdJulySlotToFifth, "Magaloof"))

[<Test>]
let ``add with date range with year and note``() = 
  let thirdJulySlotToFifth = createDate 2016 7 3 |> createSlot 3
  parseOffski "add 3rd July 2016 - 5th July 2016 #Magaloof" == Add(SlotWithNote(thirdJulySlotToFifth, "Magaloof"))

[<Test>]
let ``unknown command``() = parseOffski "boom" == Unknown "boom"