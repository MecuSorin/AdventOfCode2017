namespace AdventOfCode2017

open FSharpx

module Puzzle11 =
    type Position = 
        {   row: int
            col: int }
    type Direction = N | NE | NW | S | SE | SW

    let moveInDirection direction pos =
        match direction with
        | N ->  { pos with row = pos.row + 1 }
        | NE -> { pos with                    col = pos.col + 1 }
        | NW -> { pos with row = pos.row + 1; col = pos.col - 1 }
        | S ->  { pos with row = pos.row - 1 }
        | SE -> { pos with row = pos.row - 1; col = pos.col + 1 }
        | SW -> { pos with                    col = pos.col - 1 }

    module Parsers =
        open FParsec
        let pS = pstring   "s"  >>% moveInDirection S
        let pSE = pstring  "se" >>% moveInDirection SE
        let pSW = pstring  "sw" >>% moveInDirection SW
        let pN = pstring "n"    >>% moveInDirection N
        let pNE = pstring "ne"  >>% moveInDirection NE
        let pNW = pstring "nw"  >>% moveInDirection NW
        let pDirection = 
            spaces 
            >>. sepBy (choice [pNE; pNW; pN; pSE; pSW; pS]) (spaces >>. pchar ',' .>> spaces)
        let readMovements input =
            match run pDirection input with
            | Success (result, _, _) -> ChoiceBase.Success result
            | Failure (reason, _, _) -> Fail reason

    let getDisplacementFromZero pos =
        match pos.row = 0, pos.col = 0 with
        | true, _ -> abs pos.col
        | _, true -> abs pos.row
        | _, _ ->
            let m = min (abs pos.row) (abs pos.col)
            let M = max (abs pos.row) (abs pos.col)  
            match pos.row > 0, pos.col > 0 with
            | false, false                  // need to use abs 
            | true, true ->
                2 * m + M - m       // m + M
            | true, false
            | false, true ->
                m + M - m       // M

    let getDistance (input: string) =
        Parsers.readMovements input
        |> Choice.map (List.fold (fun p m -> m p) { row = 0; col =0})
        |> Choice.map getDisplacementFromZero

    let getMaximumDistance (input: string) =
        let folder (pos, maxDistance) movement =
            let newPos = movement pos
            let newMaxDistance = max maxDistance (getDisplacementFromZero newPos)
            newPos, newMaxDistance
        Parsers.readMovements input
        |> Choice.map (List.fold folder ({ row = 0; col =0}, -2))
        |> Choice.map (fun (_p, result) -> result)

module Puzzle11Tests =
    open Expecto
    open Puzzle11

    let basePuzzle111Test testFunction label sample expected = testFunction label <| fun _ ->
        getDistance sample
        |> expectSuccess "failed to understand the input"
        ==? expected
    let basePuzzle112Test testFunction label sample expected = testFunction label <| fun _ ->
        getMaximumDistance sample
        |> expectSuccess "failed to understand the input"
        ==? expected

    [<Tests>]
    let samples =
        testList
            "Puzzle11 "
            [   testList
                    "Samples 1"
                    [   basePuzzle111Test testCase "Sample ne,ne,ne" "ne,ne,ne" 3
                        basePuzzle111Test testCase "Sample ne,ne,sw,sw" "ne,ne,sw,sw" 0
                        basePuzzle111Test testCase "Sample se,sw,se,sw,sw" "se,sw,se,sw,sw" 3
                        basePuzzle111Test testCase "Sample ne,ne,s,s" "ne,ne,s,s" 2 ]
            ]


    let Puzzle11Sample = readFile "day11Input" 
    [<Tests>]
    let Puzzle11 =
        testList 
            "Puzzle11 "
            [   basePuzzle111Test testCase "1" Puzzle11Sample 784
                basePuzzle112Test testCase "2" Puzzle11Sample 1558 ]