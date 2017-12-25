namespace AdventOfCode2017

open System
open FSharpx
open FSharpx

module Puzzle19 =
    type Position = 
        { row: int
          col: int }
    type Direction = U | D | L | R
    type Grid = string []

    let getChar (grid: Grid) pos = 
        if pos.row < 0 || pos.row >= grid.Length
             || pos.col < 0 || pos.col >= grid.[pos.row].Length
        then None
        else Some <| grid.[pos.row].Chars(pos.col)

    let getNextPosition  pos direction =
        match direction with
        | D -> { pos with row = pos.row + 1}
        | U -> { pos with row = pos.row - 1}
        | L -> { pos with col = pos.col - 1}
        | R -> { pos with col = pos.col + 1}

    let oppositeDirection = function
        | D -> U
        | U -> D
        | L -> R
        | R -> L

    let switchDirection (grid: Grid) pos direction =
        [D; U; L; R]
        |> List.map (fun d -> d, getNextPosition pos d |> getChar grid)
        |> List.tryFind(fun (d, c) -> (c |> Option.defaultValue ' ') <> ' ' && d <> oppositeDirection direction)
        |> Option.map fst
        |> Option.defaultValue direction

    let getLetters input = 
        let grid = input |> String.split ['\n'; '\r']
        let rec trail lastPosition lastDirection lettersAcc lastSteps =
            let currentPosition = getNextPosition lastPosition lastDirection
            let currentChar = getChar grid currentPosition
            let steps = lastSteps + 1
            match currentChar with
            | None
            | Some ' ' -> steps, lettersAcc
            | Some '|'
            | Some '-' -> trail currentPosition lastDirection lettersAcc steps
            | Some '+' -> trail currentPosition (switchDirection grid currentPosition lastDirection) lettersAcc steps
            | Some c -> trail currentPosition lastDirection (c::lettersAcc) steps
        let initialPos = { row = 0; col = grid.[0].IndexOf('|') }
        trail initialPos D [] 0
        |> fun (s, letters) ->
            s, letters |> List.rev |> List.toArray |> String

module Puzzle19Tests =
    open Expecto
    open Puzzle19

    let basePuzzle191Test testFunction label sample expected = testFunction label <| fun _ ->
        expected ==? getLetters sample

    let sample = readFile "day19Sample"

    [<Tests>]
    let samples =
        testList
            "Puzzle19 "
            [   testList
                    "Samples 1"
                    [   basePuzzle191Test testCase "Sample " sample (38, "ABCDEF") ]
            ]


    let Puzzle19Sample = readFile "day19Input"
    [<Tests>]
    let Puzzle19 =
        testList 
            "Puzzle19 "
            [   basePuzzle191Test testCase "1" Puzzle19Sample (17628,"GEPYAWTMLK") ]