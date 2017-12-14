namespace AdventOfCode2017

module Puzzle2 =
    let parseString (input: string) = 
        input.Split([|'\n'|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s -> 
            s.Split([|' '; '\t'|], System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int)
    
    let public extremeDiff input = 
        parseString input
        |> Array.sumBy (fun s ->
            let max = Array.max s
            let min = Array.min s
            max - min )

    let divisors items nr =
        items
        |> Array.choose (fun t ->
            if nr = 1 || nr = t
            then None
            elif 0 = t % nr 
            then Some (t / nr)
            // elif 0 = nr % t
            // then Some nr / t
            else None)
        |> Array.tryHead


    let getTheDividersScale (numbers: int []) =
        numbers
        |> Array.choose (divisors numbers)
        |> Array.tryHead
        |> Option.defaultValue 0

    let sumOfDividers input = 
        parseString input
        |> Array.sumBy getTheDividersScale


module Puzzle2Tests =
    open Expecto
    open Puzzle2

    let basePuzzle21Test testFunction label expected sample= testFunction label <| fun _ ->
        expected ==? extremeDiff sample
    let basePuzzle22Test testFunction label sample expected = testFunction label <| fun _ ->
        expected ==? sumOfDividers sample

    [<Tests>]
    let samples =
        testList 
            "Puzzle 2"
            [   testList 
                    "Samples 1" 
                    [   basePuzzle21Test testCase "Sample 5 1 9 5" 8 "5 1 9 5"
                        basePuzzle21Test testCase "Sample 7 5 3" 4 "7 5 3"
                        basePuzzle21Test testCase "Sample 2 4 6 8" 6 "2 4 6 8" ]
                testList
                    "Samples 2"
                    [   basePuzzle22Test testCase "Sample 5 9 2 8" "5 9 2 8" 4
                        basePuzzle22Test testCase "Sample 9 4 7 3" "9 4 7 3" 3
                        basePuzzle22Test testCase "Sample 3 8 6 5" "3 8 6 5" 2 ]
            ]

    let puzzle2Sample = readFile "day02Input"
   
    [<Tests>]
    let puzzle2 =
        testList
            "Puzzle 2" 
            [   basePuzzle21Test testCase "1" 50376 puzzle2Sample
                basePuzzle22Test testCase "2" puzzle2Sample 267 ]