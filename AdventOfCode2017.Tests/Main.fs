module AdventOfCode2017Tests

open Expecto
// open AdventOfCode2017.Common

// [<Tests>]
// let tests =
//     testCase "Universe exists" <| fun _ ->
//         Expect.equal (AdventOfCode().Label) "F# |> I ❤" "Failed to wire the thing up"

[<EntryPoint>]
let main argv =
    runTestsInAssembly defaultConfig argv
