module Handlers

let createDtoFromUserIdAndOptions userId (slot : Slot) = 
  { When = System.DateTime(slot.When.Year, slot.When.Month, slot.When.Day)
    Days = slot.Days
    Note = null
    UserId = userId }

let createDto createDtoFromUserIdAndOptions = 
  function 
  | userId, Slot slot -> createDtoFromUserIdAndOptions userId slot
  | userId, SlotWithNote(slot, note) -> { createDtoFromUserIdAndOptions userId slot with Note = note }
  | _ -> failwith "cannot create DTO from this"

let handle userId command store = 
  match command with
  | Add options -> store (userId, options)
  ()

let store dbStore = createDto createDtoFromUserIdAndOptions >> dbStore
