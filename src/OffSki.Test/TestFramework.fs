﻿[<AutoOpen>]
module TestFramework

open NUnit.Framework

let private assrt assertion actual expected = assertion(box expected, box actual, sprintf "Expected: %+A\nActual: %+A" expected actual)
let (==) actual expected = assrt Assert.AreEqual actual expected 
let (!=) actual expected = assrt Assert.AreNotEqual actual expected 
 
type Test = TestAttribute