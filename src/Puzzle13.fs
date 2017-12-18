namespace AdventOfCode2017

open System
open FSharpx

module Puzzle13 =
//2: 0 1 0 1
//4: 0 1 2 3 2 1 0 1 2 3 2 1 0
    let scannerPosition range timeFrame =
        let cycleSize = range + range - 2
        let modulo = timeFrame % cycleSize
        if modulo < range
            then modulo
            else cycleSize - modulo

    let readInput = 
        String.split ['\n'; '\r']
        >> Array.map (String.split [' '; ':'])
        >> Array.map (fun r -> int r.[0], int r.[1])

    let evaluateSeverity delay firewallConfig =
        firewallConfig
        |> Array.sumBy(fun (deep, range) ->
            let timeFrame = delay + deep
            if 0 = scannerPosition range timeFrame
            then range * timeFrame
            else 0)
    
    let sneak firewallConfig =
        let rec tryWithDelay delay =
            if 0 = evaluateSeverity delay firewallConfig
            then delay
            else tryWithDelay <| delay + 1
        tryWithDelay 0
        
module Puzzle13Tests =
    open Expecto
    open Puzzle13

    let basePuzzle131Test testFunction label sample expected = testFunction label <| fun _ ->
        readInput sample
        |> evaluateSeverity 0
        ==? expected
    let basePuzzle132Test testFunction label sample expected = testFunction label <| fun _ ->
        readInput sample
        |> sneak 
        ==? expected 

    let puzzleSample = readFile "day13Sample"
    [<Tests>]
    let samples =
        testList
            "Puzzle13 "
            [   testList
                    "Samples 1"
                    [   basePuzzle131Test testCase "Sample " puzzleSample 24 ]
                testList
                    "Samples 2"
                    [   basePuzzle132Test testCase "Sample " puzzleSample 10 ]
            ]


    let Puzzle13Sample = readFile "day13Input" 
    [<Tests>]
    let Puzzle13 =
        testList 
            "Puzzle13 "
            [   basePuzzle131Test testCase "1" Puzzle13Sample 1588
                basePuzzle132Test testCase "2" Puzzle13Sample 3865118 ]