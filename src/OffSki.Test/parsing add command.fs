module ``parsing add command``

open System
open Parser

let createSlot (days : int) (from : Date) = 
  { Slot.When = from
    Days = days }

let createDay = dateTimeToDate >> createSlot 1
let today = DateTime.Today |> createDay
let tomorrow = DateTime.Today.AddDays 1. |> createDay
let nextYear = DateTime.Today.Year + 1

[<Test>]
let ``add tomorrow with note``() = parseOffski "add tomorrow #hospital with son" == Add(SlotWithNote(tomorrow, "hospital with son"))

[<Test>]
let ``add tomorrow without note``() = parseOffski "add tomorrow" == Add(Slot tomorrow)

[<Test>]
let ``add today with note``() = parseOffski "add today #hospital" == Add(SlotWithNote(today, "hospital"))

[<Test>]
let ``add with complex date``() =
  let firstJanuarySlot = createDate nextYear 1 1 |> createSlot 1
  parseOffski "add 1st January" == Add(Slot firstJanuarySlot)

[<Test>]
let ``add with date range``() = 
  let firstToThirdJanuarySlot = createDate nextYear 1 1 |> createSlot 3
  parseOffski "add 1st January - 3rd January" == Add(Slot firstToThirdJanuarySlot)

[<Test>]
let ``add with date range with note``() = 
  let firstToThirdJanuarySlot = createDate nextYear 1 1 |> createSlot 3
  parseOffski "add 1st January - 3rd January #Magaloof with the lads" == Add(SlotWithNote(firstToThirdJanuarySlot, "Magaloof with the lads"))

[<Test>]
let ``add with date range with year and note``() = 
  let firstToThirdJanuarySlot = createDate 2016 1 1 |> createSlot 3
  parseOffski "add 1st January 2016 - 3rd January #Magaloof" == Add(SlotWithNote(firstToThirdJanuarySlot, "Magaloof"))

[<Test>]
let ``unknown command``() = parseOffski "boom" == Unknown "boom"