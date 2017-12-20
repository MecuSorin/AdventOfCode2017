namespace AdventOfCode2017

open System
open FSharpx

module Puzzle15 =
    let createGenerator factor seed =
        let unfolder last =
            let value = last * factor % 2147483647UL
            Some (value, value)
        Seq.unfold unfolder seed

    let getLower16Bits no = no &&& 0xFFFFUL
    let filterOnlyValuesDivisibleBy divisor =
        let lowerBitsMask = ~~~(divisor - 1UL)
        Seq.filter(fun v -> v = (v &&& lowerBitsMask))
    let similarResults seed1 seed2  =
        createGenerator 16807UL seed1
        |> Seq.zip (createGenerator 48271UL seed2)
        |> Seq.take 40000000
        |> Seq.sumBy (fun (a, b) -> if getLower16Bits a = getLower16Bits b then 1 else 0)

    let similarResultsWithFiltered (seed1, divisor1) (seed2, divisor2)  =
        createGenerator 16807UL seed1
        |> filterOnlyValuesDivisibleBy divisor1
        |> Seq.zip (createGenerator 48271UL seed2 |> filterOnlyValuesDivisibleBy divisor2)
        |> Seq.take 5000000
        |> Seq.sumBy (fun (a, b) -> if getLower16Bits a = getLower16Bits b then 1 else 0)


module Puzzle15Tests =
    open Expecto
    open Puzzle15

    let generatorTest testFn label seed factor expected = testFn label <| fun _ ->
        createGenerator factor seed
        |> Seq.take (expected |> List.length)
        |> Seq.toList
        ==? expected

    let basePuzzle151Test testFunction label seed1 seed2 expected = testFunction label <| fun _ ->
        expected ==? similarResults seed1 seed2
    let basePuzzle152Test testFunction label seed1 seed2 expected = testFunction label <| fun _ ->
        expected ==? similarResultsWithFiltered seed1 seed2

    [<Tests>]
    let samples =
        // ignored because is taking too long
        ptestList
            "Puzzle15 "
            [   testList
                    "Samples 1"
                    [   basePuzzle151Test testCase "Sample 65 8921" 65UL 8921UL 588
                        generatorTest testCase "A generator should work with 65 16807" 65UL 16807UL [ 1092455UL; 1181022009UL; 245556042UL; 1744312007UL; 1352636452UL ]
                    ]
                testList
                    "Samples 2"
                    [   basePuzzle152Test testCase "Sample 65 8921" (65UL, 4UL) (8921UL, 8UL) 309 ]
            ]

    [<Tests>]
    let Puzzle15 =
    // ignored because is taking too long
        ptestList 
            "Puzzle15 "
            [   basePuzzle151Test testCase "1" 591UL 393UL 619
                basePuzzle152Test testCase "2" (591UL, 4UL) (393UL, 8UL) 290 //60 (again don't pay enough attention to the params) 309 too high (used the seeds from the sample :( )) 619 is too high (used the answer from the puzzle 1 by mistake)
            ]