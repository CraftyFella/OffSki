module Handlers

open System.Collections.Generic

let dateToDateTime (date : Date) = System.DateTime(date.Day, date.Month, date.Year)

let createDtoFromUserIdAndOptions dateToDateTime userId (slot : Slot) =
                           {When = slot.When |> dateToDateTime
                            Days = slot.Days
                            Note = null
                            UserId = userId} 

let createDto createDtoFromUserIdAndOptions = function
    | userId, Slot slot -> createDtoFromUserIdAndOptions userId slot
    | userId, SlotWithNote (slot, note) -> {createDtoFromUserIdAndOptions userId slot with Note = note}
    | _ -> failwith "cannot create DTO from this"

let handle userId command store =
    match command with 
        | Add options -> 
            store (userId, options)
    ()

let dbStore =
    let mutable db = Dictionary<string, OffSkiDto>()
    (fun dto -> db.Add(dto.UserId, dto) )

let store dto = dto |> (createDtoFromUserIdAndOptions dateToDateTime |> createDto) |> dbStore

