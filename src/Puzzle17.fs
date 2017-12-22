namespace AdventOfCode2017

open System
open FSharpx

module Puzzle17 =
    let spinLock step = 
        let buffer = Array.init 2018 (fun _ -> 0)

        let insert position valueInserted =
            let numberOfElements = valueInserted
            let newPosition = 1 + ((position + step) % numberOfElements)
            for i = numberOfElements - 1 downto newPosition do
                buffer.[i + 1] <- buffer.[i]
            buffer.[newPosition] <- valueInserted
            
            // if valueInserted < 100 then printfn "%A" ( buffer |> Array.take (numberOfElements + 1))
            
            newPosition

        [1..2017]
        |> List.fold insert 0
        |> fun p2017 ->
            buffer.[(p2017 + 1) % 2018]

    let spinLockAfterIndexZero step =
        let getLastInsertedAfterIndex0 (position, result) numberOfElements =
            let newPosition = 1 + (position + step) % numberOfElements
            let newResult = 
                if 1 = newPosition
                    then numberOfElements
                    else result
            newPosition, newResult
        [1..50000000]
        |> List.fold getLastInsertedAfterIndex0 (0, -1)
        |> snd

module Puzzle17Tests =
    open Expecto
    open Puzzle17

    let basePuzzle171Test testFunction label sample expected = testFunction label <| fun _ ->
        expected ==? spinLock sample
    let basePuzzle172Test testFunction label sample expected = testFunction label <| fun _ ->
        expected ==? spinLockAfterIndexZero sample

    [<Tests>]
    let samples =
        testList
            "Puzzle17 "
            [   testList
                    "Samples 1"
                    [   basePuzzle171Test testCase "Sample 3" 3 638 ]
            ]


    [<Tests>]
    let Puzzle17 =
        // taking too long to keep them on
        ptestList 
            "Puzzle17 "
            [   basePuzzle171Test testCase "1" 349 640
                basePuzzle172Test testCase "2" 349 47949463 ]