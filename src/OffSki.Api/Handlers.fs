module Handlers

let createDtoFromUserIdAndSlot (userId, slot : Slot, note) = 
  { When = System.DateTime(slot.When.Year, slot.When.Month, slot.When.Day)
    Days = slot.Days
    Note = 
      match note with
      | Some s -> s
      | _ -> null
    UserId = userId }

let createDtoFromUserIdAndOptions = 
  function 
  | userId, Slot slot -> userId, slot, None
  | userId, SlotWithNote(slot, note) -> userId, slot, Some note
  | arg -> failwithf "cannot create DTO from this %+A" arg

let createUserIdAndOptions userId = 
  function
  | Add options -> userId, options

let createDto userId = createUserIdAndOptions userId >> createDtoFromUserIdAndOptions >> createDtoFromUserIdAndSlot

let handle store userId = Parser.parseOffski >> createDto userId >> store.Save
