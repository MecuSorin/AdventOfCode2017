namespace AdventOfCode2017

module Puzzle6 =

    let getMax memoryState =
        memoryState
        |> Seq.mapi(fun i v -> v, -i)
        |> Seq.max
        |> fun (v, ni) -> v, -ni
    
    let rec balance memoryState previousStates countOfCycles = 
        let current = memoryState |> Array.map string |> String.concat " "
        if previousStates |> Map.containsKey current
        then countOfCycles, Map.find current previousStates
        else
            let rec balanceCycle blocks index =
                if blocks < 1
                then ()
                else
                    let newIndex = (index + 1) % (Array.length memoryState)
                    memoryState.[newIndex] <- memoryState.[newIndex] + 1
                    balanceCycle (blocks - 1) newIndex

            let (blocks, index) = getMax memoryState
            memoryState.[index] <- 0
            balanceCycle blocks index
            balance memoryState (previousStates |> Map.add current (countOfCycles + 1)) (countOfCycles + 1)

    let countBalanceCycles (input: string) =
        let memoryInitialState = 
            input.Split([|' '; '\t'; ','|], System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int
        balance memoryInitialState Map.empty 0

module Puzzle6Tests =
    open Expecto
    open Puzzle6

    let basePuzzle61Test testFunction label sample expected = testFunction label <| fun _ ->
        expected ==? (countBalanceCycles sample |> fst)
    let basePuzzle62Test testFunction label sample expected = testFunction label <| fun _ ->
        expected ==? (countBalanceCycles sample |> fun (a, b) -> 1 + a - b)

    [<Tests>]
    let samples =
        testList
            "Puzzle6 "
            [   testList
                    "Samples 1"
                    [   basePuzzle61Test testCase "Sample 0, 2, 7, and 0" "0, 2, 7, 0" 5 ]
                testList
                    "Samples 2"
                    [   basePuzzle62Test testCase "Sample 2 4 1 2" "2 4 1 2" 4 ]
            ]


    let Puzzle6Sample = "14 	0	15	12	11	11	3	5	1	6	8	4	9	1	8	4"
    [<Tests>]
    let Puzzle61 =
        testList 
            "Puzzle6 "
            [   basePuzzle61Test testCase "1" Puzzle6Sample 11137
                basePuzzle62Test testCase "2" Puzzle6Sample 1037 ]