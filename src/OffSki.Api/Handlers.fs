module Handlers

let createDtoFromUserIdSlotAndNote (userId, slot : Slot, note) = 
  { When = System.DateTime(slot.When.Year, slot.When.Month, slot.When.Day)
    Days = slot.Days
    Note = 
      match note with
      | Some s -> s
      | _ -> null
    UserId = userId }

let getUserIdSlotAndNote = 
  function 
  | userId, Slot slot -> userId, slot, None
  | userId, SlotWithNote(slot, note) -> userId, slot, Some note
  | arg -> failwithf "cannot create DTO from this %+A" arg

let getUserIdAndOptions userId = 
  function
  | Add options -> userId, options

let handle store userId = Parser.parseOffski >> getUserIdAndOptions userId >> getUserIdSlotAndNote >> createDtoFromUserIdSlotAndNote >> store.Save
