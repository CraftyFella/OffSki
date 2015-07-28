[<AutoOpen>]
module Domain

open System

type Date =
    { Year : int
      Month : int
      Day : int }

type Slot =
    { When : Date
      Days : int }

type OffSkiDto =
    { When : DateTime
      Days : int
      Note : string
      UserId : string }

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

type toOffSkiDto = UserId * Options -> OffSkiDto
type fromOffSkiDto = OffSkiDto -> UserId * Options
type StoreSlot = OffSkiDto -> Result<unit>
type RetrieveSlots = UserId -> Result<OffSkiDto seq>