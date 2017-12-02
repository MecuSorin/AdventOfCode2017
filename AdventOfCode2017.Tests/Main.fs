module AdventOfCode2017.Tests

open Expecto
open AdventOfCode2017

[<Tests>]
let tests =
    testCase "Universe exists" <| fun _ ->
        Expect.equal (AdventOfCode().Label) "F# |> I ❤" "Failed to wire the thing up"

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
