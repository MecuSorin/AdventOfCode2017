namespace AdventOfCode2017

module Puzzle5 =
    let part1OffsetUpdater jumpSize = jumpSize + 1
    let part2OffsetUpdater jumpSize =
        if jumpSize > 2
        then jumpSize - 1
        else jumpSize + 1

    let countStepsToFreedom offsetUpdater (input: string) =
        let maze = 
            input.Split([|'\n'; ' '|], System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.filter (fun s -> not <| System.String.IsNullOrWhiteSpace(s))
            |> Array.map (fun s -> s.Trim() |> int)
        let mazeSize = maze.Length
        let rec traverseMaze steps positionIndex =
            if positionIndex < 0 || positionIndex >= mazeSize
            then steps
            else
                let jumpSize = maze.[positionIndex]
                maze.[positionIndex] <- offsetUpdater jumpSize
                traverseMaze  (steps + 1) (positionIndex + jumpSize)
        traverseMaze 0 0

module Puzzle5Tests =
    open Expecto
    open Puzzle5

    let basePuzzle51Test testFunction label sample expected = testFunction label <| fun _ ->
        expected ==? countStepsToFreedom part1OffsetUpdater sample
    let basePuzzle52Test testFunction label sample expected = testFunction label <| fun _ ->
        expected ==? countStepsToFreedom part2OffsetUpdater sample

    [<Tests>]
    let samples =
        testList
            "Puzzle5 "
            [   testList
                    "Samples 1"
                    [   basePuzzle51Test testCase "Sample (0) 3  0  1  -3" "0 3  0  1  -3" 5 ]
                testList
                    "Samples 2"
                    [   basePuzzle52Test testCase "Sample (0) 3  0  1  -3" "0 3  0  1  -3" 10 ]
            ]


    let Puzzle5Sample = readFile "day05Input" 
    [<Tests>]
    let Puzzle51 =
        testList 
            "Puzzle5 "
            [   basePuzzle51Test testCase "1" Puzzle5Sample 358131    /// not 646
                basePuzzle52Test testCase "2" Puzzle5Sample 25558839
            ]