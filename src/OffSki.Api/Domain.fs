[<AutoOpen>]
module Domain

type Date =
    { Year : int
      Month : int
      Day : int }

type Slot =
    { When : Date
      Days : int }

type Options =
  | Unknown of string
  | Slot of Slot
  | SlotWithNote of Slot * string
  | Empty
  | Invalid

type Command =
  | Add of Options
  | List of Options
  | Unknown of string

let createDate year month day =
    { Year = year
      Month = month
      Day = day }

type UserId = UserId of string

type Result<'a> = Success of 'a | Failure of string

type StoreSlot = UserId -> Slot -> Result<unit>
type RetrieveSlots = UserId -> Result<Slot seq>
