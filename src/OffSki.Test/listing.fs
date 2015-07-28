module ``listing offskis``

module ``user with 2 offskis`` = 
  open System.Collections.Generic
  
  //"add today #hospital" |> 
  //"add tomorrow #ibiza"
  let inMemoryStore = 
    let mutable db = Dictionary<string, OffSkiDto>()
    (fun dto -> db.Add(dto.UserId, dto))
  
  [<Test>]
  let ``offskis are returned``() = 
    let userId, message = ("Dave", "add tomorrow #Holiday")
    let command = Parser.parseOffski (message)
    Handlers.store inMemoryStore |> Handlers.handle userId command