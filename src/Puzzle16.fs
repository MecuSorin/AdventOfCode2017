namespace AdventOfCode2017

open System
open FSharpx

module Puzzle16 =
    type DanceMove= 
        | Spin of int
        | Exchange of int * int
        | Partner of char * char
    type State = string
    module Parsers =
        open FParsec

        let pSpin = pchar 's' >>. pint32 |>> Spin
        let pExchange = pchar 'x' >>. pint32 .>> pchar '/' .>>. pint32 |>> Exchange
        let pPartner = pchar 'p' >>. letter .>> pchar '/' .>>. letter |>> Partner
        let readDanceMoves = 
            run (sepBy (choice [pSpin; pExchange; pPartner]) (pchar ',')) 
            >> Choice.fromParserResult

    let swap (state: string) a b =
        state.Replace(a, '_').Replace(b, a).Replace('_', b)

    let dance (state: string) = function
        | Spin items ->
            let splitPosition = state.Length - items
            sprintf "%s%s" (state.Substring splitPosition) (state.Substring (0, splitPosition))
        | Exchange (a, b) ->
            //suboptimal implementation
            swap state (state.Chars a) (state.Chars b)
        | Partner (a, b) -> 
            swap state a b


    let applyDanceUnit moves state = 
        moves |> List.fold dance state

    let danceTillEnd endLetter moves =
        String([|'a' .. endLetter|])
        |> applyDanceUnit moves

    let killLegsDancing initialState moves =
        let rec loopUntilCycle loops (state: State)  =
            if state = initialState && loops <> 0
            then loops
            else 
                state |> applyDanceUnit moves
                |> loopUntilCycle (loops + 1)
        let cycleSize = loopUntilCycle 0 initialState
        printfn "%d" cycleSize
        [1 .. 1000000000 % cycleSize]
        |> List.fold (fun s _ -> applyDanceUnit moves s) initialState

module Puzzle16Tests =
    open Expecto
    open Puzzle16

    let basePuzzle161Test testFunction label endLetter sample expected = testFunction label <| fun _ ->
        Parsers.readDanceMoves sample
        |> expectSuccess "Readed the dance moves"
        |> danceTillEnd endLetter
        ==? expected
    let basePuzzle162Test testFunction label sample expected = testFunction label <| fun _ ->
        Parsers.readDanceMoves sample
        |> expectSuccess "Readed the dance moves"
        |> killLegsDancing (String([|'a' .. 'p'|]))
        ==? expected 

    [<Tests>]
    let samples =
        testList
            "Puzzle16 "
            [   testList
                    "Samples 1"
                    [   basePuzzle161Test testCase "Sample s1,x3/4,pe/b" 'e' "s1,x3/4,pe/b" "baedc" ]
            ]


    let Puzzle16Sample = readFile "day16Input"
    [<Tests>]
    let Puzzle16 =
        ptestList 
            "Puzzle16 "
            [   basePuzzle161Test testCase "1" 'p' Puzzle16Sample "gkmndaholjbfcepi"
                basePuzzle162Test testCase "2" Puzzle16Sample "abihnfkojcmegldp" ]
