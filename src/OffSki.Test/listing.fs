module ``listing offskis``

open System
open System.Text.RegularExpressions
open Chronic
open Parser

module ``user with 2 offskis`` =
    open NUnit.Framework

    //"add today #hospital" |> 
    //"add tomorrow #ibiza"

    [<Test>]
    let ``offskis are returned``() = 
        let userId, message = ("Dave", "add tomorrow #Holiday")
        let command = Parser.parseOffski(message)
        Handlers.handle userId command Handlers.store
        0
    