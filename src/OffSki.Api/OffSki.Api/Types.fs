[<AutoOpen>]
module Domain

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

type UserId = UserId of string

type Slot = 
    { When : Date
      Days : int }


type Result<'a> = Success of 'a | Failure of string

type StoreSlot = UserId -> Slot -> Result<unit>
type RetrieveSlots = UserId -> Result<Slot seq>
