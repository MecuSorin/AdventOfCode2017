namespace AdventOfCode2017

module Puzzle3 =
    type Direction = 
        | Up
        | Down
        | Left
        | Right

    type Position = 
        {   h: int 
            v: int }
        with 
            static member root = { h = 0; v = 0}
            static member stepsToTheRoot position =
                abs(position.h) + abs(position.v)

    type Square = 
        {   width: int
            max: int
            level: int }
        with
            static member Root = 
                {   width = 1
                    max = 1
                    level = 0 }

            member x.NextSquare () =
                let newWidth = x.width + 2
                {   width = newWidth
                    max = x.max + 4 * newWidth  - 4
                    level = x.level + 1 }

    let move steps direction position =
        match direction with
        | Up -> { position with v = position.v + steps}
        | Down -> { position with v = position.v - steps}
        | Left -> { position with h = position.h + steps}
        | Right -> { position with h = position.h - steps}

    let calculateMovement (nr: int) =
        let rec loop square =
            if nr > square.max
            then loop <| square.NextSquare()
            else
                let corner1 = square.max - square.width + 1
                let corner2 = corner1 - square.width + 1
                let corner3 = corner2 - square.width + 1
                let maxPosition = 
                    Position.root
                    |> move square.level Up
                    |> move square.level Left

                if nr > corner1
                then 
                    maxPosition
                    |> move (square.max - nr) Right
                elif nr > corner2
                then
                    maxPosition
                    |> move (square.width - 1) Right
                    |> move (corner1 - nr) Down
                elif nr > corner3
                then
                    maxPosition
                    |> move (square.width - 1) Right
                    |> move (square.width - 1) Down
                    |> move (corner2 - nr) Left
                else
                    maxPosition
                    |> move (square.width - 1) Right
                    |> move (square.width - 1) Down
                    |> move (square.width - 1) Left
                    |> move (corner3 - nr) Up
        loop Square.Root
        |> Position.stepsToTheRoot


module Puzzle3Tests =
    open Expecto
    open Puzzle3

    let (==?) expected actual = Expect.equal actual expected "Should be equal"

    let basePuzzle31Test testFunction label expected sample = testFunction label <| fun _ ->
        expected ==? calculateMovement sample

    [<Tests>]
    let samples = 
        testList 
            "Puzzle 3"
            [   testList 
                    "Samples 1"
                    [   basePuzzle31Test testCase "Sample 1" 0 1
                        basePuzzle31Test testCase "Sample 12" 3 12
                        basePuzzle31Test testCase "Sample 23" 2 23
                        basePuzzle31Test testCase "Sample 1024" 31 1024 ]
            ]

    [<Tests>]
    let debug = 
        testList 
            "Puzzle 3" 
            [
                testList 
                    "Debug 1/Square lvl 1"
                    [
                        basePuzzle31Test testCase "Debug 9" 2 9
                        basePuzzle31Test testCase "Debug 7" 2 7
                        basePuzzle31Test testCase "Debug 5" 2 5
                        basePuzzle31Test testCase "Debug 3" 2 3
                        basePuzzle31Test testCase "Debug 2" 1 2
                    ]
                testList 
                    "Debug 1/Square lvl 2"
                    [
                        basePuzzle31Test testCase "Debug 24" 3 24
                        basePuzzle31Test testCase "Debug 25" 4 25
                        basePuzzle31Test testCase "Debug 21" 4 21
                        basePuzzle31Test testCase "Debug 17" 4 17
                        basePuzzle31Test testCase "Debug 13" 4 13
                    ]
            ]
    [<Tests>]
    let puzzle3 =
        testList 
            "Puzzle 3"
            [   basePuzzle31Test testCase "1" 480 347991 ]