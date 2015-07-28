module ``listing offskis``

module ``user with 2 offskis`` = 
  open Domain
  open System
  
  let inMemoryStore = 
    let db = ResizeArray<OffSkiDto>()
    { SlotStore.Save = 
        fun dto -> 
          db.Add dto
          Success()
      Retrieve = 
        fun userId -> 
          db
          |> Seq.filter (fun dto -> dto.UserId = userId)
          |> Success }
  
  [<Test>]
  let ``offskis are stored``() = 
    Handlers.handle inMemoryStore "Dave" "add 26th July 2015 #Holiday" == Success()
    let (Success offski) = inMemoryStore.Retrieve "Dave"
    offski |> Seq.head == {When = DateTime(2015, 7, 26); Days = 1; Note = "Holiday"; UserId = "Dave" }
