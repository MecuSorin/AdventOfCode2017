namespace AdventOfCode2017

module Puzzle4 =
    let validPassphrase arrayMorpher (input: string) = 
        let words = input.Split([|' '; '\t'; '\r'|], System.StringSplitOptions.RemoveEmptyEntries)
        words
        |> arrayMorpher
        |> Array.distinct
        |> Array.length
        |> (=) (Array.length words)

    let part1Morpher = id
    let part2Morpher (a: string[]): string[] = 
        Array.map (Seq.sort >> Seq.toArray >> System.String) a
    
    let countValidPassPhrases arrayMorpher (input: string) = 
        input.Split([|'\n'|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.sumBy (fun row -> if validPassphrase arrayMorpher row then 1 else 0)

module Puzzle4Tests =
    open Expecto
    open Puzzle4

    let basePuzzle41Test testFunction label sample expected = testFunction label <| fun _ ->
        expected ==? validPassphrase part1Morpher sample
    let basePuzzle42Test testFunction label sample expected = testFunction label <| fun _ ->
        expected ==? validPassphrase part2Morpher sample

    [<Tests>]
    let samples =
        testList
            "Puzzle 4"
            [   testList
                    "Samples 1"
                    [   basePuzzle41Test testCase "Sample aa bb cc dd ee" "aa bb cc dd ee" true
                        basePuzzle41Test testCase "Sample aa bb cc dd aa" "aa bb cc dd aa" false
                        basePuzzle41Test testCase "Sample aa bb cc dd aaa" "aa bb cc dd aaa" true ]
                testList
                    "Samples 2"
                    [   basePuzzle42Test testCase "Sample abcde fghij" "abcde fghij" true
                        basePuzzle42Test testCase "Sample abcde xyz ecdab" "abcde xyz ecdab" false
                        basePuzzle42Test testCase "Sample a ab abc abd abf abj" "a ab abc abd abf abj" true
                        basePuzzle42Test testCase "Sample iiii oiii ooii oooi oooo" "iiii oiii ooii oooi oooo" true
                        basePuzzle42Test testCase "Sample oiii ioii iioi iiio" "oiii ioii iioi iiio" false ]
            ]


    let puzzle4Sample = readFile "day04Input"
    [<Tests>]
    let puzzle1 =
        testList 
            "Puzzle 4"
            [   testCase "1" <| fun _ -> 466 ==? (countValidPassPhrases part1Morpher puzzle4Sample)
                testCase "2" <| fun _ -> 251 ==? (countValidPassPhrases part2Morpher puzzle4Sample) ]