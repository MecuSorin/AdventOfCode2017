namespace AdventOfCode2017

module Puzzle =

module PuzzleTests =
    open Expecto
    open Puzzle

    let basePuzzle1Test testFunction label sample expected = testFunction label <| fun _ ->
        expected ==?  sample
    // let basePuzzle2Test testFunction label sample expected = testFunction label <| fun _ ->
    //     expected ==?  sample

    [<Tests>]
    let samples =
        testList
            "Puzzle "
            [   testList
                    "Samples 1"
                    [   basePuzzle1Test testCase "Sample "
                        basePuzzle1Test testCase "Sample "
                        basePuzzle1Test testCase "Sample " ]
                // testList
                //     "Samples 2"
                //     [   basePuzzle2Test testCase "Sample "
                //         basePuzzle2Test testCase "Sample "
                //         basePuzzle2Test testCase "Sample " ]
            ]


    // let puzzleSample = 
    // [<Tests>]
    // let puzzle =
    //     testList 
    //         "Puzzle "
    //         [   basePuzzle1Test testCase "1" puzzleSample
    //             basePuzzle2Test testCase "2" puzzleSample
    //         ]